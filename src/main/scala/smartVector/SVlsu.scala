package smartVector
import chisel3._
import chisel3.util._
import darecreek.VDecode
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import darecreek.exu.vfu.VUop
import xiangshan.MicroOp
import SmartParam._

class VLSUOutput extends Bundle {
    val vd = UInt(VLEN.W)
    val uopQueueIdx = UInt(4.W) // magic number
    val vxsat = Bool()
}

class LdstIO(implicit p: Parameters) extends ParameterizedBundle()(p) {
    val mUop = Flipped(ValidIO(new UopQueueOutput()(p)))
    val lsuOut = ValidIO(new VLSUOutput)
    val dataExchange = new RVUMemory()
    val lsuReady = Output(Bool())
}

class LdstUop extends Bundle {
    val valid = Bool()
    val addr = Output(UInt(64.W))
    val effecSize = Output(UInt(3.W))
    val offset = Output(UInt(6.W))
}

object VRegSegmentStatus {
  val invalid :: notReady :: ready :: Nil = Enum(3)
}

class VRegSegmentInfo extends Bundle {
    // 0: not ready, 1: ready
    val status = UInt(log2Ceil(3).W)
    // corresponding ldstuop idx of current vreg segement
    val idx = UInt(8.W)
    // offset of writeback valid data for current vreg segement
    val offset = UInt(6.W)
    // data of current vreg segement
    val data = UInt(8.W)
}

class SVlsu(implicit p: Parameters) extends Module {
    val io = IO(new LdstIO())

    val uop_idle :: uop_split :: uop_split_finish :: uop_complete :: Nil = Enum(4)
    val uopState = RegInit(uop_idle)
    val completeLd = RegInit(false.B)

    when(uopState === uop_idle) {
        // LSU free --> accept new uop request
        io.lsuReady := true.B
    }.otherwise {
        io.lsuReady := false.B
    }

    // define v reg info
    var ldUopSize = 16
    val ldUopIdxBits = log2Up(ldUopSize)
    val vRegIdx = Wire(UInt(5.W))
    val vregInfo = RegInit(VecInit(Seq.fill(16)(0.U.asTypeOf(new VRegSegmentInfo))))

    // SPLIT FSM
    when(uopState === uop_idle) {
        when(io.mUop.valid) {
            uopState := uop_split
        }.otherwise {
            uopState := uop_idle
        }
    }.elsewhen(uopState === uop_split) {
        uopState := uop_split_finish
    }.elsewhen(uopState === uop_split_finish) {
        when(completeLd) {
            uopState := uop_complete
        }.otherwise {
            uopState := uop_split_finish
        }   
    }.otherwise {
        uopState := uop_idle
    }

    /**********************SPLIT STAGE**************************/
    // stage 0 IDLE --> stage 1 SPLIT (start to split) --> stage 2 SPLIT_FINISH (split finish) --> stage 3 COMPLETE (writeback to uopQueue)
    vRegIdx := io.mUop.bits.uopAttribute.ldest
    val s1_mUop = RegNext(io.mUop)

    val ldstEnqPtr = RegInit(0.U(ldUopIdxBits.W))
    val ldstUopQueue = RegInit(VecInit(Seq.fill(ldUopSize)(0.U.asTypeOf(new LdstUop))))

    when(uopState === uop_split) {
        // unit stride
        ldstUopQueue(ldstEnqPtr).valid := true.B
        ldstUopQueue(ldstEnqPtr).addr := s1_mUop.bits.scalar_opnd_1
        // split into lduop queue & write vreginfo
        for(i <- 0 until 16) {
            vregInfo(i).status := VRegSegmentStatus.notReady
            vregInfo(i).idx := ldstEnqPtr
            vregInfo(i).offset := (15.U - i.U)
        }
        ldstEnqPtr := ldstEnqPtr + 1.U
    }
    
    /*-----------------SPLIT STAGE END-----------------------*/


    /******************ISSUE STAGE START**********************/
    // issue stage
    // issue lduop to hellacache

    val issueLdstUop = WireInit(0.U.asTypeOf(new LdstUop))
    val issueLdstPtr = RegInit(0.U(ldUopIdxBits.W))

    val ld_idle :: ld_issue :: ld_wait :: ld_complete :: ld_replay :: Nil = Enum(5)
    
    //prepare hellacache req
    val dataExchangeState = RegInit(ld_idle)

    when(dataExchangeState === ld_idle) {
        // ldstuop queue has data & hellacache is not busy
        when(ldstUopQueue(issueLdstPtr).valid && !io.dataExchange.busy) {
            dataExchangeState := ld_issue
        }.otherwise {
            dataExchangeState := ld_idle
        }
    }.elsewhen(dataExchangeState === ld_issue) {
        dataExchangeState := ld_wait
    }.elsewhen(dataExchangeState === ld_wait) {
        when(io.dataExchange.resp.valid && io.dataExchange.resp.bits.has_data) {
            dataExchangeState := ld_complete
        } .elsewhen(io.dataExchange.resp.valid && io.dataExchange.resp.bits.replay) {
            dataExchangeState := ld_replay
        }.otherwise {
            dataExchangeState := ld_wait
        }
    }.elsewhen(dataExchangeState === ld_complete) {
        dataExchangeState := ld_idle
    }.elsewhen(dataExchangeState === ld_replay) {
        dataExchangeState := ld_issue
    }.otherwise {
        dataExchangeState := ld_idle
    }

    when(dataExchangeState === ld_issue) {
        issueLdstUop := ldstUopQueue(issueLdstPtr)
        io.dataExchange.req.valid := true.B
        io.dataExchange.req.bits.addr := issueLdstUop.addr
        io.dataExchange.req.bits.cmd := M_XRD
        io.dataExchange.req.bits.size := log2Ceil(64).U
        io.dataExchange.req.bits.signed := false.B
        io.dataExchange.req.bits.phys := false.B
        io.dataExchange.req.bits.idx := issueLdstPtr
        io.dataExchange.req.bits.data := DontCare
        io.dataExchange.req.bits.mask := DontCare

    }.otherwise {
        io.dataExchange.req.valid := false.B
        io.dataExchange.req.bits := DontCare
    }

     

    /************************cache hit***********************/
    // cache hit after 2 cycle
    // data writeback from cache

    /*todo
        * 1. write data into dest reg
        * 2. deq lduop
    */
    when(dataExchangeState === ld_wait && io.dataExchange.resp.valid && io.dataExchange.resp.bits.has_data) {
        val loadData = io.dataExchange.resp.bits.data
        for(i <- 0 until 16) {
            when(vregInfo(i).idx === issueLdstPtr) {
                for(j <- 0 until 8) {
                    when(vregInfo(i).offset === j.U) {
                        vregInfo(i).data := loadData(j * 8 + 7, j * 8)
                    }
                }
                vregInfo(i).status := VRegSegmentStatus.ready
            }
        }
        issueLdstPtr := issueLdstPtr + 1.U
        completeLd := true.B
    }
    
    /**************************exception handling**********************************/


    /**************************cache miss replay***********************************/



    /************************** Ldest data writeback to uopQueue********************/
    val vreg_wb_ready = Wire(Bool())
    vreg_wb_ready := vregInfo.forall(info => info.status === VRegSegmentStatus.ready || info.status === VRegSegmentStatus.invalid)

    when(vreg_wb_ready) {
        io.lsuOut.valid := true.B
        io.lsuOut.bits.vd := Cat(vregInfo.map(_.data)) // Concatenate data from all vregInfo elements
        io.lsuOut.bits.uopQueueIdx := 0.U // Adjust this value based on your design
        io.lsuOut.bits.vxsat := false.B

        // Reset vreg info
        for (i <- 0 until 16) {
            vregInfo(i).status := VRegSegmentStatus.invalid
            vregInfo(i).idx := DontCare
            vregInfo(i).offset := DontCare
            vregInfo(i).data := DontCare
        }
    }.otherwise {
        io.lsuOut.valid := false.B
        io.lsuOut.bits := DontCare // You may want to provide default values for other bits
    }

}
