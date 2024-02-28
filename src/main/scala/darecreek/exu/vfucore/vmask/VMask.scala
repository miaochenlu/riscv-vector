package darecreek.exu.vfucore.vmask

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import darecreek.exu.vfucore._
// import darecreek.exu.vfucore.VFUParam._
import chipsalliance.rocketchip.config._

class VMask(implicit p: Parameters) extends VFuModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new VFuInput))
    val out = Decoupled(new VMaskOutput)
  })

  val uop = io.in.bits.uop
  val vs1 = io.in.bits.vs1
  val vs2 = io.in.bits.vs2
  val old_vd = io.in.bits.oldVd
  val vmask = io.in.bits.mask
  val rs1 = io.in.bits.rs1
  val funct6 = uop.ctrl.funct6
  val funct3 = uop.ctrl.funct3
  val imme = uop.ctrl.lsrc(0)
  val vs1_index = uop.ctrl.lsrc(0)
  val vs2_index = uop.ctrl.lsrc(1)
  val vl = io.in.bits.uop.info.vl
  val vstart = io.in.bits.uop.info.vstart
  val uopIdx = io.in.bits.uop.uopIdx
  val uopEnd = io.in.bits.uop.uopEnd
  val vm = uop.ctrl.vm
  val vlmul = uop.info.vlmul
  val vsew = uop.info.vsew
  val ma = uop.info.ma
  val ta = uop.info.ta
  val fire = io.in.fire

  val vmand_mm = (funct6 === "b011001".U) && (funct3 === "b010".U)
  val vmnand_mm = (funct6 === "b011101".U) && (funct3 === "b010".U)
  val vmandn_mm = (funct6 === "b011000".U) && (funct3 === "b010".U)
  val vmxor_mm = (funct6 === "b011011".U) && (funct3 === "b010".U)
  val vmor_mm = (funct6 === "b011010".U) && (funct3 === "b010".U)
  val vmnor_mm = (funct6 === "b011110".U) && (funct3 === "b010".U)
  val vmorn_mm = (funct6 === "b011100".U) && (funct3 === "b010".U)
  val vmxnor_mm = (funct6 === "b011111".U) && (funct3 === "b010".U)

  val vcpop_m = (funct6 === "b010000".U) && (funct3 === "b010".U) && (vs1_index === "b10000".U)
  val vfirst_m = (funct6 === "b010000".U) && (funct3 === "b010".U) && (vs1_index === "b10001".U)
  val vmsbf_m = (funct6 === "b010100".U) && (funct3 === "b010".U) && (vs1_index === "b00001".U)
  val vmsif_m = (funct6 === "b010100".U) && (funct3 === "b010".U) && (vs1_index === "b00011".U)
  val vmsof_m = (funct6 === "b010100".U) && (funct3 === "b010".U) && (vs1_index === "b00010".U)
  val viota_m = (funct6 === "b010100".U) && (funct3 === "b010".U) && (vs1_index === "b10000".U)
  val vid_v = (funct6 === "b010100".U) && (funct3 === "b010".U) && (vs1_index === "b10001".U)

  val vm_logical = vmand_mm ||
    vmnand_mm ||
    vmandn_mm ||
    vmxor_mm ||
    vmor_mm ||
    vmnor_mm ||
    vmorn_mm ||
    vmxnor_mm

  val vmand = Wire(UInt(VLEN.W))
  val vmandn = Wire(UInt(VLEN.W))
  val vmxor = Wire(UInt(VLEN.W))
  val vmor = Wire(UInt(VLEN.W))
  val vmorn = Wire(UInt(VLEN.W))
  val vmnand = Wire(UInt(VLEN.W))
  val vmnor = Wire(UInt(VLEN.W))
  val vmxnor = Wire(UInt(VLEN.W))

  vmand := vs2 & vs1
  vmandn := vs2 & ~vs1
  vmxor := vs2 ^ vs1
  vmor := vs2 | vs1
  vmorn := vs2 | ~vs1
  vmnand := ~vmand
  vmnor := ~vmor
  vmxnor := ~vmxor

  val eew = SewOH(vsew)
  val vsew_plus1 = Wire(UInt(7.W))
  vsew_plus1 := Cat(0.U(1.W), ~vsew(1, 0)) + (log2Up(VLEN) - 6).U
  val vsew_bytes = Mux1H(eew.oneHot, Seq(1.U(4.W), 2.U(4.W), 4.U(4.W), 8.U(4.W)))
  val vsew_bits = Mux1H(eew.oneHot, Seq(8.U(7.W), 16.U(7.W), 32.U(7.W), 64.U(7.W)))
  val ele_cnt = Mux1H(eew.oneHot, Seq((VLEN / 8).U, (VLEN / 16).U, (VLEN / 32).U, (VLEN / 64).U))
  val vlRemain = Wire(UInt(bVL.W))
  val vlRemainBytes = vlRemain << vsew
  val all_one = (~0.U(VLEN.W))

  val vmfirst = Wire(SInt(XLEN.W))
  val vmsbf = Wire(UInt(VLEN.W))
  val vmsif = Cat(vmsbf(VLEN - 2, 0), 1.U)
  val vmsof = Wire(UInt(VLEN.W))
  val nmask = Wire(UInt(VLEN.W))
  val vd_nmask = Wire(UInt(VLEN.W))
  val vd_mask = Wire(UInt(VLEN.W))
  val vs2m = Wire(Vec(VLEN, UInt(1.W)))

  def sbf(data: UInt): UInt = {
    val w = data.getWidth
    val result = Wire(UInt(w.W))
    if (w == 1) {
      result := Mux(data(0), 0.U(1.W), 1.U(1.W))
    } else {
      val hi = sbf(data(w - 1, w / 2))
      val lo = sbf(data(w / 2 - 1, 0))
      result := Mux(lo(w / 2 - 1), Cat(hi, lo), Cat(0.U((w / 2).W), lo))
    }
    result
  }

  def vfirst(data: UInt): UInt = {
    val w = data.getWidth
    val logW = log2Ceil(w) // 1 -> 0, 2 -> 1, 4 -> 2
    val result = Wire(UInt((logW + 1).W))
    if (w == 1) {
      result := Mux(data(0), 0.U(1.W), 1.U(1.W))
    } else {
      val hi = vfirst(data(w - 1, w / 2))
      val lo = vfirst(data(w / 2 - 1, 0))
      result := Mux(!lo(logW - 1), Cat(0.U(1.W), lo),
        if (w == 2) Cat(hi(logW - 1), 1.U(1.W)) else Cat(hi(logW - 1), 1.U(1.W), hi(logW - 2, 0)))
    }
    result
  }

  vlRemain := Mux(vl >= (uopIdx << vsew_plus1), vl - (uopIdx << vsew_plus1), 0.U)

  for (i <- 0 until VLEN) {
    vs2m(i) := 0.U
    when(fire) {
      vs2m(i) := vs2(i) & (vmask(i) | vm) & (i.U < vl)
    }
  }

  vmsof := ~vmsbf & vmsif
  vmsbf := sbf(Cat(vs2m.reverse))
  when(!Cat(vs2m.reverse).orR) {
    vmfirst := (-1.S(XLEN.W))
  }.otherwise {
    vmfirst := vfirst(Cat(vs2m.reverse)).asSInt
  }

  // viota/vid/vcpop
  val vs2m_uop = MaskExtract(Cat(vs2m.reverse), uopIdx, eew)
  val vs2m_uop_vid = Mux(vid_v, Fill(vlenb, vid_v), vs2m_uop)
  val one_sum = RegInit(0.U(bVL.W))
  val one_cnt = Wire(Vec(vlenb + 1, UInt(8.W)))
  val one_cnt_uop = Wire(Vec(vlenb + 1, UInt(8.W)))
  val vid_vd = Wire(Vec(vlenb, UInt(8.W)))
  val vid_vd_sew8 = Wire(Vec(vlenb, UInt(8.W)))
  val vid_vd_sew16 = Wire(Vec(vlenb, UInt(8.W)))
  val vid_vd_sew32 = Wire(Vec(vlenb, UInt(8.W)))
  val vid_vd_sew64 = Wire(Vec(vlenb, UInt(8.W)))

  for (i <- 0 until vlenb + 1) {
    one_cnt_uop(i) := 0.U
  }

  for (i <- 0 until vlenb) {
    one_cnt_uop(i + 1) := PopCount(vs2m_uop_vid(i, 0))
  }

  when(fire) {
    one_sum := one_cnt(ele_cnt)
  }

  for (i <- 0 until vlenb + 1) {
    one_cnt(i) := Mux(uopIdx === 0.U, one_cnt_uop(i), one_sum + one_cnt_uop(i))
  }

  val tail_vd = Wire(UInt(VLEN.W))
  val vd_reg = RegInit(0.U(VLEN.W))
  val vd_out = Wire(UInt(VLEN.W))

  when(vmand_mm && fire) {
    vd_reg := vmand
  }.elsewhen(vmnand_mm && fire) {
    vd_reg := vmnand
  }.elsewhen(vmandn_mm && fire) {
    vd_reg := vmandn
  }.elsewhen(vmxor_mm && fire) {
    vd_reg := vmxor
  }.elsewhen(vmor_mm && fire) {
    vd_reg := vmor
  }.elsewhen(vmnor_mm && fire) {
    vd_reg := vmnor
  }.elsewhen(vmorn_mm && fire) {
    vd_reg := vmorn
  }.elsewhen(vmxnor_mm && fire) {
    vd_reg := vmxnor
  }.elsewhen(vmsbf_m && fire) {
    vd_reg := vmsbf
  }.elsewhen(vmsif_m && fire) {
    vd_reg := vmsif
  }.elsewhen(vmsof_m && fire) {
    vd_reg := vmsof
  }.elsewhen(vfirst_m && fire) {
    vd_reg := Cat(0.U((VLEN - XLEN).W), vmfirst)
  }.elsewhen((vid_v || viota_m) && fire) {
    vd_reg := Cat(one_cnt.reverse)(VLEN - 1, 0)
  }.elsewhen(vcpop_m && fire) {
    vd_reg := one_cnt(ele_cnt)
  }

  val vstartRemain = Wire(UInt(bVL.W))
  vstartRemain := Mux(vid_v, Mux(vstart >= (uopIdx << vsew_plus1), (vstart - (uopIdx << vsew_plus1)), 0.U), 0.U)

  val vl_reg = RegEnable(vl, 0.U, fire)
  val vstart_reg = RegEnable(vstart, 0.U, fire)
  val vm_reg = RegEnable(vm, false.B, fire)
  val ma_reg = RegEnable(ma, false.B, fire)
  val ta_reg = RegEnable(ta, false.B, fire)
  val vsew_reg = RegEnable(vsew, 0.U, fire)
  val uopIdx_reg = RegEnable(uopIdx, 0.U, fire)
  val vlRemainBytes_reg = RegEnable(vlRemainBytes, 0.U, fire)
  val vstartRemain_reg = RegEnable(vstartRemain, 0.U, fire)
  val old_vd_reg = RegEnable(old_vd, 0.U, fire)
  val vmask_reg = RegEnable(vmask, 0.U, fire)
  val reg_vm_logical = RegEnable(vm_logical, false.B, fire)
  val reg_vcpop_m = RegEnable(vcpop_m, false.B, fire)
  val reg_vfirst_m = RegEnable(vfirst_m, false.B, fire)
  val reg_vmsbf_m = RegEnable(vmsbf_m, false.B, fire)
  val reg_vmsif_m = RegEnable(vmsif_m, false.B, fire)
  val reg_vmsof_m = RegEnable(vmsof_m, false.B, fire)
  val reg_viota_m = RegEnable(viota_m, false.B, fire)
  val reg_vid_v = RegEnable(vid_v, false.B, fire)
  val vsew_plus1_reg = Wire(UInt(3.W))
  vsew_plus1_reg := Cat(0.U(1.W), ~vsew_reg(1, 0)) + 1.U

  val vmask_bits = Wire(UInt(VLEN.W))
  vmask_bits := vmask_reg >> (uopIdx_reg << vsew_plus1_reg)
  val vmask_vd_bytes = Wire(Vec(vlenb, UInt(8.W)))
  val vmask_vd_bits = Cat(vmask_vd_bytes.reverse)
  val vmask_old_vd = old_vd_reg & (~vmask_vd_bits)
  val vmask_ones_vd = all_one & (~vmask_vd_bits)
  val vmask_vd = Mux(ma_reg, vmask_ones_vd, vmask_old_vd)
  val vid_mask_vd = (Cat(vid_vd.reverse) & vmask_vd_bits) | vmask_vd

  val vstartRemainBytes = Wire(UInt(bVL.W))
  val vstart_bytes = Wire(UInt(bVL.W))
  vstartRemainBytes := vstartRemain_reg << vsew_reg
  vstart_bytes := Mux(vstartRemainBytes >= vlenb.U, vlenb.U, vstartRemainBytes)
  val vstart_bits = Cat(vstart_bytes, 0.U(3.W))
  val vmask_vstart_bits = Wire(UInt(VLEN.W))
  vmask_vstart_bits := all_one << vstart_bits
  val vstart_old_vd = old_vd_reg & (~vmask_vstart_bits)

  val tail_bytes = Mux((vlRemainBytes_reg >= vlenb.U), 0.U, vlenb.U - vlRemainBytes_reg)
  val tail_bits = Cat(tail_bytes, 0.U(3.W))
  val vmask_tail_bits = Wire(UInt(VLEN.W))
  vmask_tail_bits := all_one >> tail_bits
  val tail_old_vd = old_vd_reg & (~vmask_tail_bits)
  val tail_ones_vd = ~vmask_tail_bits
  val vid_tail_vd = Mux(ta_reg, tail_ones_vd, tail_old_vd)
  val vid_tail_mask_vd = Wire(UInt(VLEN.W))
  val vd_reg_byte = VecInit(Seq.tabulate(vlenb)(i => vd_reg((i + 1) * 8 - 1, i * 8)))

  for (i <- 0 until vlenb) {
    vid_vd(i) := 0.U
    vid_vd_sew8(i) := 0.U
    vid_vd_sew16(i) := 0.U
    vid_vd_sew32(i) := 0.U
    vid_vd_sew64(i) := 0.U
  }

  for (i <- 0 until vlenb) {
    vid_vd_sew8(i) := vd_reg_byte(i)
  }

  for (i <- 0 until vlenb) {
    when(i.U % 2.U === 0.U) {
      vid_vd_sew16(i) := vd_reg_byte(i / 2)
    }.otherwise {
      vid_vd_sew16(i) := 0.U
    }
  }

  for (i <- 0 until vlenb) {
    when(i.U % 4.U === 0.U) {
      vid_vd_sew32(i) := vd_reg_byte(i / 4)
    }.otherwise {
      vid_vd_sew32(i) := 0.U
    }
  }
  for (i <- 0 until vlenb) {
    when(i.U % 8.U === 0.U) {
      vid_vd_sew64(i) := vd_reg_byte(i / 8)
    }.otherwise {
      vid_vd_sew64(i) := 0.U
    }
  }

  for (i <- 0 until vlenb) {
    when(vsew_reg === 0.U) {
      vid_vd(i) := vid_vd_sew8(i)
    }.elsewhen(vsew_reg === 1.U) {
      vid_vd(i) := vid_vd_sew16(i)
    }.elsewhen(vsew_reg === 2.U) {
      vid_vd(i) := vid_vd_sew32(i)
    }.elsewhen(vsew_reg === 3.U) {
      vid_vd(i) := vid_vd_sew64(i)
    }
  }

  for (i <- 0 until vlenb) {
    vmask_vd_bytes(i) := "hff".U
    when((!vm_reg && !vmask_bits(i.U >> vsew_reg)) || (i.U >= vlRemainBytes_reg)) {
      vmask_vd_bytes(i) := 0.U
    }
  }

  vid_tail_mask_vd := 0.U
  when(reg_vid_v || reg_viota_m) {
    vid_tail_mask_vd := (vid_mask_vd & vmask_tail_bits & vmask_vstart_bits) | vid_tail_vd | vstart_old_vd
  }

  nmask := ~(vmask_reg | Fill(VLEN, vm_reg))
  vd_nmask := Mux(ma_reg, nmask, old_vd_reg & nmask)
  vd_mask := vd_reg & (vmask_reg | Fill(VLEN, vm_reg))
  val mask_vd = vd_nmask | vd_mask

  val old_vd_vl_mask = Wire(UInt(VLEN.W))
  val vd_vl_mask = Wire(UInt(VLEN.W))
  val vstart_mask = Wire(UInt(VLEN.W))
  val vstart_vd = Wire(UInt(VLEN.W))

  old_vd_vl_mask := (~0.U(VLEN.W)) << vl_reg
  vd_vl_mask := (~0.U(VLEN.W)) >> (VLEN.U - vl_reg)

  vstart_mask := (~0.U(VLEN.W)) << vstart_reg
  vstart_vd := old_vd_reg & ~vstart_mask

  tail_vd := vstart_vd | old_vd_vl_mask | (mask_vd & vd_vl_mask & vstart_mask)

  vd_out := vd_reg
  when((vstart_reg >= vl_reg) && !reg_vfirst_m && !reg_vcpop_m) {
    vd_out := old_vd_reg
  }.elsewhen(reg_vm_logical || reg_vmsbf_m || reg_vmsif_m || reg_vmsof_m) {
    vd_out := tail_vd
  }.elsewhen(reg_vid_v || reg_viota_m) {
    vd_out := vid_tail_mask_vd
  }

  val out_valid = RegInit(false.B)

  when(fire) {
    out_valid := true.B
  }.elsewhen(io.out.valid && io.out.ready) {
    out_valid := false.B
  }

  //--------- Ready & valid ---------
  io.in.ready := (!io.in.valid || io.out.ready)
  io.out.valid := out_valid
  io.out.bits.vd := vd_out

  io.out.bits.uop := RegEnable(uop, fire)

}

object MaskExtract {
  def VLEN = 256

  def apply(vmask: UInt, uopIdx: UInt, sew: SewOH) = {
    val extracted = Wire(UInt((VLEN / 8).W))
    extracted := Mux1H(Seq.tabulate(8)(uopIdx === _.U),
      Seq.tabulate(8)(idx => Mux1H(sew.oneHot, Seq(VLEN / 8, VLEN / 16, VLEN / 32, VLEN / 64).map(stride =>
        vmask((idx + 1) * stride - 1, idx * stride)))))
    extracted
  }
}

import xiangshan._

object VerilogVMask extends App {
  println("Generating hardware")
  val p = Parameters.empty
  emitVerilog(new VMask()(p.alterPartial({ case VFuParamsKey =>
    VFuParameters(VLEN = 256)
  })), Array("--target-dir", "generated",
    "--emission-options=disableMemRandomization,disableRegisterRandomization"))
}