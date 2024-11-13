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
  val int = Bool() // Integer RF
  val fp = Bool() // Floating-point RF
  val vec = Bool() // Vector RF
  val vsb_id = UInt(bVScoreboardId.W)
  val waddr = UInt(5.W)
  val wdata = UInt(xLen.W) // int/fp, wdata of enq is not available for long-latency instrn
  //TODO - add csr
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
  val rob_wb = Input(Vec(NRET, ValidIO(new ROBWb))) // int/fp, wb is only for long-latency instrn
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
  rob_enq_swapped(0) := Mux(swapEnq, io.uvm_in.rob_enq(1).bits, io.uvm_in.rob_enq(0).bits)
  rob_enq_swapped(1) := Mux(swapEnq, io.uvm_in.rob_enq(0).bits, io.uvm_in.rob_enq(1).bits)
  rob_enq_swapped_valid(0) := Mux(swapEnq, io.uvm_in.rob_enq(1).valid, io.uvm_in.rob_enq(0).valid)
  rob_enq_swapped_valid(1) := Mux(swapEnq, io.uvm_in.rob_enq(0).valid, io.uvm_in.rob_enq(1).valid)
  when(rob_enq_swapped_valid(0)) {
    debugROB(enqPtrROB.value).valid := true.B
    debugROB(enqPtrROB.value).pc := rob_enq_swapped(0).pc
    debugROB(enqPtrROB.value).insn := rob_enq_swapped(0).insn
    debugROB(enqPtrROB.value).int := rob_enq_swapped(0).int
    debugROB(enqPtrROB.value).fp := rob_enq_swapped(0).fp
    debugROB(enqPtrROB.value).vec := rob_enq_swapped(0).vec
    debugROB(enqPtrROB.value).vsb_id := rob_enq_swapped(0).vsb_id
    debugROB(enqPtrROB.value).waddr := rob_enq_swapped(0).waddr
    debugROB(enqPtrROB.value).wdata := rob_enq_swapped(0).wdata
    debugROB(enqPtrROB.value).ready_to_commit := true.B //FIXME - 
  }
  when(rob_enq_swapped_valid(0) && rob_enq_swapped_valid(1)) {
    debugROB((enqPtrROB + 1.U).value).valid := true.B
    debugROB((enqPtrROB + 1.U).value).pc := rob_enq_swapped(1).pc
    debugROB((enqPtrROB + 1.U).value).insn := rob_enq_swapped(1).insn
    debugROB((enqPtrROB + 1.U).value).int := rob_enq_swapped(1).int
    debugROB((enqPtrROB + 1.U).value).fp := rob_enq_swapped(1).fp
    debugROB((enqPtrROB + 1.U).value).vec := rob_enq_swapped(1).vec
    debugROB((enqPtrROB + 1.U).value).vsb_id := rob_enq_swapped(1).vsb_id
    debugROB((enqPtrROB + 1.U).value).waddr := rob_enq_swapped(1).waddr
    debugROB((enqPtrROB + 1.U).value).wdata := rob_enq_swapped(1).wdata
    debugROB((enqPtrROB + 1.U).value).ready_to_commit := true.B //FIXME - 
  }
  when(rob_enq_swapped_valid(0) && rob_enq_swapped_valid(1)) {
    enqPtrROB := enqPtrROB + 2.U
  }.elsewhen(rob_enq_swapped_valid(0)) {
    enqPtrROB := enqPtrROB + 1.U
  }

  // Wb of ROB
  //TODO - 

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
  io.uvm_out.reg_fpr := Cat(emul_int_FRF_next.map(frf => Cat(frf.tail.reverse)).reverse)

  val old_csr = RegEnable(io.uvm_in.csr, commit_valids.reduce(_ || _))
  val csr_swap = RegEnable(io.uvm_in.swap, commit_valids.reduce(_ || _))

  io.uvm_out.csr.mstatus      := Mux(csr_swap, Cat(io.uvm_in.csr.mstatus , old_csr.mstatus ), Cat(io.uvm_in.csr.mstatus ,io.uvm_in.csr.mstatus ))
  io.uvm_out.csr.mepc         := Mux(csr_swap, Cat(io.uvm_in.csr.mepc    , old_csr.mepc    ), Cat(io.uvm_in.csr.mepc    ,io.uvm_in.csr.mepc    ))
  io.uvm_out.csr.mtval        := Mux(csr_swap, Cat(io.uvm_in.csr.mtval   , old_csr.mtval   ), Cat(io.uvm_in.csr.mtval   ,io.uvm_in.csr.mtval   ))
  io.uvm_out.csr.mtvec        := Mux(csr_swap, Cat(io.uvm_in.csr.mtvec   , old_csr.mtvec   ), Cat(io.uvm_in.csr.mtvec   ,io.uvm_in.csr.mtvec   ))
  io.uvm_out.csr.mcause       := Mux(csr_swap, Cat(io.uvm_in.csr.mcause  , old_csr.mcause  ), Cat(io.uvm_in.csr.mcause  ,io.uvm_in.csr.mcause  ))
  io.uvm_out.csr.mip          := Mux(csr_swap, Cat(io.uvm_in.csr.mip     , old_csr.mip     ), Cat(io.uvm_in.csr.mip     ,io.uvm_in.csr.mip     ))
  io.uvm_out.csr.mie          := Mux(csr_swap, Cat(io.uvm_in.csr.mie     , old_csr.mie     ), Cat(io.uvm_in.csr.mie     ,io.uvm_in.csr.mie     ))
  io.uvm_out.csr.mscratch     := Mux(csr_swap, Cat(io.uvm_in.csr.mscratch, old_csr.mscratch), Cat(io.uvm_in.csr.mscratch,io.uvm_in.csr.mscratch))
  io.uvm_out.csr.mideleg      := Mux(csr_swap, Cat(io.uvm_in.csr.mideleg , old_csr.mideleg ), Cat(io.uvm_in.csr.mideleg ,io.uvm_in.csr.mideleg ))
  io.uvm_out.csr.medeleg      := Mux(csr_swap, Cat(io.uvm_in.csr.medeleg , old_csr.medeleg ), Cat(io.uvm_in.csr.medeleg ,io.uvm_in.csr.medeleg ))
  io.uvm_out.csr.sstatus      := Mux(csr_swap, Cat(io.uvm_in.csr.sstatus , old_csr.sstatus ), Cat(io.uvm_in.csr.sstatus ,io.uvm_in.csr.sstatus ))
  io.uvm_out.csr.sepc         := Mux(csr_swap, Cat(io.uvm_in.csr.sepc    , old_csr.sepc    ), Cat(io.uvm_in.csr.sepc    ,io.uvm_in.csr.sepc    ))
  io.uvm_out.csr.stval        := Mux(csr_swap, Cat(io.uvm_in.csr.stval   , old_csr.stval   ), Cat(io.uvm_in.csr.stval   ,io.uvm_in.csr.stval   ))
  io.uvm_out.csr.stvec        := Mux(csr_swap, Cat(io.uvm_in.csr.stvec   , old_csr.stvec   ), Cat(io.uvm_in.csr.stvec   ,io.uvm_in.csr.stvec   ))
  io.uvm_out.csr.scause       := Mux(csr_swap, Cat(io.uvm_in.csr.scause  , old_csr.scause  ), Cat(io.uvm_in.csr.scause  ,io.uvm_in.csr.scause  ))
  io.uvm_out.csr.satp         := Mux(csr_swap, Cat(io.uvm_in.csr.satp    , old_csr.satp    ), Cat(io.uvm_in.csr.satp    ,io.uvm_in.csr.satp    ))
  io.uvm_out.csr.sscratch     := Mux(csr_swap, Cat(io.uvm_in.csr.sscratch, old_csr.sscratch), Cat(io.uvm_in.csr.sscratch,io.uvm_in.csr.sscratch))
  io.uvm_out.csr.vtype        := Mux(csr_swap, Cat(io.uvm_in.csr.vtype   , old_csr.vtype   ), Cat(io.uvm_in.csr.vtype   ,io.uvm_in.csr.vtype   ))
  io.uvm_out.csr.vcsr         := Mux(csr_swap, Cat(io.uvm_in.csr.vcsr    , old_csr.vcsr    ), Cat(io.uvm_in.csr.vcsr    ,io.uvm_in.csr.vcsr    ))
  io.uvm_out.csr.vl           := Mux(csr_swap, Cat(io.uvm_in.csr.vl      , old_csr.vl      ), Cat(io.uvm_in.csr.vl      ,io.uvm_in.csr.vl      ))
  io.uvm_out.csr.vstart       := Mux(csr_swap, Cat(io.uvm_in.csr.vstart  , old_csr.vstart  ), Cat(io.uvm_in.csr.vstart  ,io.uvm_in.csr.vstart  ))


  io.uvm_out.csr.minstret := Cat(io.uvm_in.csr.minstret, io.uvm_in.csr.minstret - commit_valids(1))

  val pass = RegInit(false.B)
  when((commit_valids(0) && commit_bits(0).insn === (0x6b.U(32.W))) ||
    (commit_valids(1) && commit_bits(1).insn === (0x6b.U(32.W)))) {
    pass := true.B
  }

  io.uvm_out.sim_halt := pass
}