package gpc.core

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import VectorParam._
import utility._

class VerInCSR(implicit p: Parameters) extends CoreBundle()(p) {
  val mstatus = UInt((xLen).W)
  val mepc = UInt((xLen).W)
  val mtval = UInt((xLen).W)
  val mtvec = UInt((xLen).W)
  val mcause = UInt((xLen).W)
  val mip = UInt((xLen).W)
  val mie = UInt((xLen).W)
  val mscratch = UInt((xLen).W)
  val mideleg = UInt((xLen).W)
  val medeleg = UInt((xLen).W)
  val minstret = UInt((xLen).W)
  val sstatus = UInt((xLen).W)
  val sepc = UInt((xLen).W)
  val stval = UInt((xLen).W)
  val stvec = UInt((xLen).W)
  val scause = UInt((xLen).W)
  val satp = UInt((xLen).W)
  val sscratch = UInt((xLen).W)
  val vtype = UInt((xLen).W)
  val vcsr = UInt((xLen).W)
  val vl = UInt((xLen).W)
  val vstart = UInt((xLen).W)
}

class VerOutCSR(implicit p: Parameters) extends CoreBundle()(p) {
  val mstatus = UInt((NRET * xLen).W)
  val mepc = UInt((NRET * xLen).W)
  val mtval = UInt((NRET * xLen).W)
  val mtvec = UInt((NRET * xLen).W)
  val mcause = UInt((NRET * xLen).W)
  val mip = UInt((NRET * xLen).W)
  val mie = UInt((NRET * xLen).W)
  val mscratch = UInt((NRET * xLen).W)
  val mideleg = UInt((NRET * xLen).W)
  val medeleg = UInt((NRET * xLen).W)
  val minstret = UInt((NRET * xLen).W)
  val sstatus = UInt((NRET * xLen).W)
  val sepc = UInt((NRET * xLen).W)
  val stval = UInt((NRET * xLen).W)
  val stvec = UInt((NRET * xLen).W)
  val scause = UInt((NRET * xLen).W)
  val satp = UInt((NRET * xLen).W)
  val sscratch = UInt((NRET * xLen).W)
  val vtype = UInt((NRET * xLen).W)
  val vcsr = UInt((NRET * xLen).W)
  val vl = UInt((NRET * xLen).W)
  val vstart = UInt((NRET * xLen).W)
}

class VerOutIO(implicit p: Parameters) extends CoreBundle()(p) {
  val commit_valid = Output(UInt(NRET.W))
  val commit_currPc = Output(UInt((NRET * xLen).W))
  val commit_insn = Output(UInt((NRET * 32).W))

  val sim_halt = Output(Bool())

  val trap_valid = Output(Bool())
  val trap_code = Output(UInt((xLen).W))

  val reg_gpr = Output(UInt((NRET * 31 * xLen).W))
  val reg_fpr = Output(UInt((NRET * 32 * fLen).W))
  val reg_vpr = Output(UInt((NRET * 32 * vLen).W))

  val csr = Output(new VerOutCSR)
}

class ROBEnq(implicit p: Parameters) extends CoreBundle()(p) {
  val pc = UInt(xLen.W)
  val insn = UInt(32.W)
  val raw_insn = UInt(32.W)
  val rvc = Bool() // Integer
  val int = Bool() // Integer RF
  val fp = Bool() // Floating-point RF
  val vec = Bool() // Vector RF
  val vsb_id = UInt(bVScoreboardId.W)
  val waddr = UInt(5.W)
  val wdata = UInt(xLen.W) // int/fp, wdata of enq is not available for long-latency instrn
  val ll_ind = Bool() // Long latency indication
}

class ROBWb(implicit p: Parameters) extends CoreBundle()(p) {
  val int = Bool() // Integer RF
  val fp = Bool() // Floating-point RF
  val waddr = UInt(5.W)
  val wdata = UInt(xLen.W) // int/fp, wb is only for long-latency instrn
}

class VerInIO(implicit p: Parameters) extends CoreBundle()(p) {
  val swap = Input(Bool())
  val rob_enq = Input(Vec(NRET, ValidIO(new ROBEnq)))
  val m2_xcpt = Input(Vec(NRET, Bool()))
  val m2_cause = Input(Vec(NRET, UInt(xLen.W)))
  //  val rob_wb = Input(Vec(NRET, ValidIO(new ROBWb))) // int/fp, wb is only for long-latency instrn
  val ll_int_wr = Input(Vec(NRET, Bool())) // Long latency int back: int load miss for pipe0 and int div for pipe1
  val ll_int_waddr = Input(Vec(NRET, UInt(5.W)))
  val ll_int_wdata = Input(Vec(NRET, UInt(xLen.W)))
  val ll_fp_wr = Input(Vec(NRET, Bool())) // Long latency fp dmem for pipe0, fp fma/div/sqrt for pipe1
  val ll_fp_waddr = Input(Vec(NRET, UInt(5.W)))
  val ll_fp_wdata = Input(Vec(NRET, UInt(xLen.W)))
  val csr = Input(new VerInCSR)

  //TODO - add other signals such as csr and vector wb
}

class ROBEntry(implicit p: Parameters) extends CoreBundle()(p) {
  val valid = Bool()
  val pc = UInt(xLen.W)
  val insn = UInt(32.W)
  val int = Bool() // Integer
  val fp = Bool() // Floating-point
  val vec = Bool() // Vector
  val vsb_id = UInt(bVScoreboardId.W)
  val waddr = UInt(5.W)
  val wdata = UInt(xLen.W) // int/fp, wdata of enq is not available for long-latency instrn
  val csr = (new VerInCSR())
  val ready_to_commit = Bool()
  //TODO - add csr and vector wdata
}

class UvmVerification(implicit p: Parameters) extends CoreModule {
  val io = IO(new Bundle {
    val uvm_in = new VerInIO
    val uvm_out = new VerOutIO
  })

  val ROBSize = 64
  val debugROB = Reg(Vec(ROBSize, new ROBEntry))

  class robPtr extends CircularQueuePtr[robPtr](ROBSize)

  val enqPtrROB = RegInit(0.U.asTypeOf(new robPtr))
  val deqPtrROB = RegInit(0.U.asTypeOf(new robPtr))
  when(reset.asBool) {
    debugROB.foreach(_.valid := false.B)
  }

  // Enq of ROB
  val swapEnq = io.uvm_in.swap
  val rob_enq_swapped = Wire(Vec(2, new ROBEnq))
  val rob_enq_swapped_valid = Wire(Vec(2, Bool()))
  val ready_to_commit_mux = Wire(Vec(2, Bool()))
  val wdata_mux = Wire(Vec(2, UInt(xLen.W)))
  val csr_swapped = Wire(Vec(2, new(VerInCSR)))
  val old_csr = RegEnable(io.uvm_in.csr, rob_enq_swapped_valid.reduce(_ || _))
  rob_enq_swapped(0) := Mux(swapEnq, io.uvm_in.rob_enq(1).bits, io.uvm_in.rob_enq(0).bits)
  rob_enq_swapped(1) := Mux(swapEnq, io.uvm_in.rob_enq(0).bits, io.uvm_in.rob_enq(1).bits)
  rob_enq_swapped_valid(0) := Mux(swapEnq, io.uvm_in.rob_enq(1).valid, io.uvm_in.rob_enq(0).valid)
  rob_enq_swapped_valid(1) := Mux(swapEnq, io.uvm_in.rob_enq(0).valid, io.uvm_in.rob_enq(1).valid)
  csr_swapped(0) := Mux(swapEnq, old_csr, io.uvm_in.csr)
  csr_swapped(0).minstret := io.uvm_in.csr.minstret - rob_enq_swapped_valid(1)
  csr_swapped(1) := io.uvm_in.csr

  // LL comes with wb
  for (i <- 0 until NRET) {
    ready_to_commit_mux(i) := rob_enq_swapped_valid(i) && !rob_enq_swapped(i).ll_ind
    wdata_mux(i) := rob_enq_swapped(i).wdata
    for (j <- 0 until NRET) {
      when(io.uvm_in.ll_int_wr(j)) {
        when(rob_enq_swapped_valid(i) && rob_enq_swapped(i).int && io.uvm_in.ll_int_waddr(j) === rob_enq_swapped(i).waddr) {
          ready_to_commit_mux(i) := true.B
          wdata_mux(i) := io.uvm_in.ll_int_wdata(j)
        }
      }
      when(io.uvm_in.ll_fp_wr(j)) {
        when(rob_enq_swapped_valid(i) && rob_enq_swapped(i).fp && io.uvm_in.ll_fp_waddr(j) === rob_enq_swapped(i).waddr) {
          ready_to_commit_mux(i) := true.B
          wdata_mux(i) := io.uvm_in.ll_fp_wdata(j)
        }
      }
    }
  }

  when(rob_enq_swapped_valid(0)) {
    debugROB(enqPtrROB.value).valid := true.B
    debugROB(enqPtrROB.value).pc := rob_enq_swapped(0).pc
    debugROB(enqPtrROB.value).insn := Mux(rob_enq_swapped(0).rvc, rob_enq_swapped(0).raw_insn, rob_enq_swapped(0).insn)
    debugROB(enqPtrROB.value).int := rob_enq_swapped(0).int
    debugROB(enqPtrROB.value).fp := rob_enq_swapped(0).fp
    debugROB(enqPtrROB.value).vec := rob_enq_swapped(0).vec
    debugROB(enqPtrROB.value).vsb_id := rob_enq_swapped(0).vsb_id
    debugROB(enqPtrROB.value).waddr := rob_enq_swapped(0).waddr
    debugROB(enqPtrROB.value).wdata := wdata_mux(0)
    debugROB(enqPtrROB.value).csr := csr_swapped(0)
    debugROB(enqPtrROB.value).ready_to_commit := ready_to_commit_mux(0)
  }
  when(rob_enq_swapped_valid(0) && rob_enq_swapped_valid(1)) {
    debugROB((enqPtrROB + 1.U).value).valid := true.B
    debugROB((enqPtrROB + 1.U).value).pc := rob_enq_swapped(1).pc
    debugROB((enqPtrROB + 1.U).value).insn := Mux(rob_enq_swapped(1).rvc, rob_enq_swapped(1).raw_insn, rob_enq_swapped(1).insn)
    debugROB((enqPtrROB + 1.U).value).int := rob_enq_swapped(1).int
    debugROB((enqPtrROB + 1.U).value).fp := rob_enq_swapped(1).fp
    debugROB((enqPtrROB + 1.U).value).vec := rob_enq_swapped(1).vec
    debugROB((enqPtrROB + 1.U).value).vsb_id := rob_enq_swapped(1).vsb_id
    debugROB((enqPtrROB + 1.U).value).waddr := rob_enq_swapped(1).waddr
    debugROB((enqPtrROB + 1.U).value).wdata := wdata_mux(1)
    debugROB((enqPtrROB + 1.U).value).csr := csr_swapped(1)
    debugROB((enqPtrROB + 1.U).value).ready_to_commit := ready_to_commit_mux(1)
  }
  when(rob_enq_swapped_valid(0) && rob_enq_swapped_valid(1)) {
    enqPtrROB := enqPtrROB + 2.U
  }.elsewhen(rob_enq_swapped_valid(0)) {
    enqPtrROB := enqPtrROB + 1.U
  }

  // Wb of LL
  for (i <- 0 until NRET) {
    when(io.uvm_in.ll_int_wr(i)) {
      for (j <- 0 until ROBSize) {
        when(debugROB(j).valid && debugROB(j).int && io.uvm_in.ll_int_waddr(i) === debugROB(j).waddr) {
          debugROB(j).ready_to_commit := true.B
          debugROB(j).wdata := io.uvm_in.ll_int_wdata(i)
        }
      }
    }
  }

  for (i <- 0 until NRET) {
    when(io.uvm_in.ll_fp_wr(i)) {
      for (j <- 0 until ROBSize) {
        when(debugROB(j).valid && debugROB(j).fp && io.uvm_in.ll_fp_waddr(i) === debugROB(j).waddr) {
          debugROB(j).ready_to_commit := true.B
          debugROB(j).wdata := io.uvm_in.ll_fp_wdata(i)
        }
      }
    }
  }

  // Commit of ROB
  val commit_valids = Wire(Vec(2, Bool()))
  val commit_wxds = Wire(Vec(2, Bool()))
  val commit_wfds = Wire(Vec(2, Bool()))
  commit_valids(0) := debugROB(deqPtrROB.value).valid && debugROB(deqPtrROB.value).ready_to_commit
  commit_valids(1) := commit_valids(0) &&
    debugROB((deqPtrROB + 1.U).value).valid && debugROB((deqPtrROB + 1.U).value).ready_to_commit
  commit_wxds(0) := debugROB(deqPtrROB.value).int
  commit_wxds(1) := debugROB((deqPtrROB + 1.U).value).int
  commit_wfds(0) := debugROB(deqPtrROB.value).fp
  commit_wfds(1) := debugROB((deqPtrROB + 1.U).value).fp

  when(commit_valids(0) && commit_valids(1)) {
    deqPtrROB := deqPtrROB + 2.U
    debugROB(deqPtrROB.value).valid := false.B
    debugROB((deqPtrROB + 1.U).value).valid := false.B
  }.elsewhen(commit_valids(0)) {
    deqPtrROB := deqPtrROB + 1.U
    debugROB(deqPtrROB.value).valid := false.B
  }
  val commit_bits = Wire(Vec(2, new ROBEntry))
  commit_bits(0) := debugROB(deqPtrROB.value)
  commit_bits(1) := debugROB((deqPtrROB + 1.U).value)

  /**
   * Emulated Integer Register File
   */
  val emul_int_RF = RegInit(VecInit(Seq.fill(32)(0.U(xLen.W))))
  val emul_int_RF_next = Wire(Vec(2, Vec(32, UInt(xLen.W))))
  emul_int_RF_next(0) := emul_int_RF
  when(commit_valids(0) && commit_wxds(0)) {
    emul_int_RF_next(0)(commit_bits(0).waddr) := commit_bits(0).wdata
  }
  emul_int_RF_next(1) := emul_int_RF_next(0)
  when(commit_valids(1) && commit_wxds(1)) {
    emul_int_RF_next(1)(commit_bits(1).waddr) := commit_bits(1).wdata
  }
  emul_int_RF := emul_int_RF_next(1)

  /**
   * Emulated Float-point Register File
   */
  val emul_int_FRF = RegInit(VecInit(Seq.fill(32)(0.U(xLen.W))))
  val emul_int_FRF_next = Wire(Vec(2, Vec(32, UInt(xLen.W))))
  emul_int_FRF_next(0) := emul_int_FRF
  when(commit_valids(0) && commit_wfds(0)) {
    emul_int_FRF_next(0)(commit_bits(0).waddr) := commit_bits(0).wdata
  }
  emul_int_FRF_next(1) := emul_int_FRF_next(0)
  when(commit_valids(1) && commit_wfds(1)) {
    emul_int_FRF_next(1)(commit_bits(1).waddr) := commit_bits(1).wdata
  }
  emul_int_FRF := emul_int_FRF_next(1)

  /**
   * Final verification interface
   */
  io.uvm_out := DontCare

  io.uvm_out.commit_valid := commit_valids.asUInt
  io.uvm_out.commit_currPc := Cat(commit_bits.map(_.pc).reverse)
  io.uvm_out.commit_insn := Cat(commit_bits.map(_.insn).reverse)
  io.uvm_out.reg_gpr := Cat(emul_int_RF_next.map(rf => Cat(rf.tail.reverse)).reverse)
  io.uvm_out.reg_fpr := Cat(emul_int_FRF_next.map(frf => Cat(frf.reverse)).reverse)

  io.uvm_out.csr.mstatus := Cat(commit_bits(1).csr.mstatus, commit_bits(0).csr.mstatus)
  io.uvm_out.csr.mepc := Cat(commit_bits(1).csr.mepc, commit_bits(0).csr.mepc)
  io.uvm_out.csr.mtval := Cat(commit_bits(1).csr.mtval, commit_bits(0).csr.mtval)
  io.uvm_out.csr.mtvec := Cat(commit_bits(1).csr.mtvec, commit_bits(0).csr.mtvec)
  io.uvm_out.csr.mcause := Cat(commit_bits(1).csr.mcause, commit_bits(0).csr.mcause)
  io.uvm_out.csr.mip := Cat(commit_bits(1).csr.mip, commit_bits(0).csr.mip)
  io.uvm_out.csr.mie := Cat(commit_bits(1).csr.mie, commit_bits(0).csr.mie)
  io.uvm_out.csr.mscratch := Cat(commit_bits(1).csr.mscratch, commit_bits(0).csr.mscratch)
  io.uvm_out.csr.mideleg := Cat(commit_bits(1).csr.mideleg, commit_bits(0).csr.mideleg)
  io.uvm_out.csr.medeleg := Cat(commit_bits(1).csr.medeleg, commit_bits(0).csr.medeleg)
  io.uvm_out.csr.minstret := Cat(commit_bits(1).csr.minstret, commit_bits(0).csr.minstret)
  io.uvm_out.csr.sstatus := Cat(commit_bits(1).csr.sstatus, commit_bits(0).csr.sstatus)
  io.uvm_out.csr.sepc := Cat(commit_bits(1).csr.sepc, commit_bits(0).csr.sepc)
  io.uvm_out.csr.stval := Cat(commit_bits(1).csr.stval, commit_bits(0).csr.stval)
  io.uvm_out.csr.stvec := Cat(commit_bits(1).csr.stvec, commit_bits(0).csr.stvec)
  io.uvm_out.csr.scause := Cat(commit_bits(1).csr.scause, commit_bits(0).csr.scause)
  io.uvm_out.csr.satp := Cat(commit_bits(1).csr.satp, commit_bits(0).csr.satp)
  io.uvm_out.csr.sscratch := Cat(commit_bits(1).csr.sscratch, commit_bits(0).csr.sscratch)
  io.uvm_out.csr.vtype := Cat(commit_bits(1).csr.vtype, commit_bits(0).csr.vtype)
  io.uvm_out.csr.vcsr := Cat(commit_bits(1).csr.vcsr, commit_bits(0).csr.vcsr)
  io.uvm_out.csr.vl := Cat(commit_bits(1).csr.vl, commit_bits(0).csr.vl)
  io.uvm_out.csr.vstart := Cat(commit_bits(1).csr.vstart, commit_bits(0).csr.vstart)

  val pass = RegInit(false.B)
  when((commit_valids(0) && commit_bits(0).insn === (0x6b.U(32.W))) ||
    (commit_valids(1) && commit_bits(1).insn === (0x6b.U(32.W)))) {
    pass := true.B
  }

  io.uvm_out.trap_valid := RegNext(RegNext(io.uvm_in.m2_xcpt.reduce(_ || _)))
  io.uvm_out.trap_code := RegNext(RegNext(Mux(io.uvm_in.m2_xcpt(0), io.uvm_in.m2_cause(0), io.uvm_in.m2_cause(1))))
  io.uvm_out.sim_halt := pass
}