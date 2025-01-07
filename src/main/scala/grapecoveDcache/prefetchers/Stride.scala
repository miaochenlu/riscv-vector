package grapecoveDcache
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.tilelink._
import grapecoveDCache.AddrDecoder.getLineAddr
import grapecoveDCache._

case class StridePrefetcherParams(
) extends CanInstantiatePrefetcher {
  def desc() = "Stride Prefetcher"
  def instantiate()(
      implicit p: Parameters
  ) = Module(new StridePrefetcher(this)(p))
}

class StrideMetaArray extends Bundle() with DCacheParams {
  val pre_hash_pc = UInt(pcWidth.W)
  val pre_vaddr   = UInt(paddrWidth.W)
  val stride      = UInt(log2Up(strideMax).W)
  val confidence  = UInt(log2Up(strideConfMax).W)

  def tag_match(valid1: Bool, valid2: Bool, new_hash_pc: UInt): Bool =
    valid1 && valid2 && pre_hash_pc === new_hash_pc

  def alloc(vaddr: UInt, alloc_hash_pc: UInt) = {
    pre_vaddr   := vaddr(vaddrWidth - 1, 0)
    stride      := 0.U
    confidence  := 0.U
    pre_hash_pc := alloc_hash_pc
  }

  def update(vaddr: UInt, always_update_pre_vaddr: Bool) = {
    val new_vaddr      = vaddr(vaddrWidth - 1, 0)
    val new_stride     = new_vaddr - pre_vaddr
    val new_stride_blk = getLineAddr(new_stride)
    // NOTE: for now, disable negtive stride
    val stride_valid   = new_stride_blk =/= 0.U && new_stride_blk =/= 1.U && new_stride(vaddrWidth - 1) === 0.U
    val stride_match   = new_stride === stride
    val low_confidence = confidence <= 1.U
    val can_send_pf    = stride_valid && stride_match && confidence === strideConfMax.U

    when(stride_valid) {
      when(stride_match) {
        confidence := Mux(confidence === strideConfMax.U, confidence, confidence + 1.U)
      }.otherwise {
        confidence := Mux(confidence === 0.U, confidence, confidence - 1.U)
        when(low_confidence) {
          stride := new_stride
        }
      }
      pre_vaddr := new_vaddr
    }
    when(always_update_pre_vaddr) {
      pre_vaddr := new_vaddr
    }

    (can_send_pf, new_stride)
  }
}

class StridePrefetcher(
    params: StridePrefetcherParams
)(
    implicit p: Parameters
) extends AbstractPrefetcher()(p) {
  val desc = "StridePrefetcher"

  val array  = Reg(Vec(strideEntryNum, new StrideMetaArray))
  val valids = RegInit(VecInit(Seq.fill(strideEntryNum)(false.B)))

  def reset_array(i: Int): Unit =
    valids(i) := false.B
  // only need to rest control signals for firendly area
  // array(i).reset(i)

  val replacement = ReplacementPolicy.fromString("plru", strideEntryNum)

  def pc_hash_tag(x: UInt): UInt = {
    val tagShift  = 5.U
    val setMask   = Fill(24, 1.U(1.W))
    val hashWidth = setMask.getWidth

    val hash1: UInt = (x >> 1).asUInt
    val hash2: UInt = (hash1 >> tagShift).asUInt

    (hash1 ^ hash2) & setMask
  }

  // s0: hash pc -> cam all entries
  val s0_can_accept   = Wire(Bool())
  val s0_valid        = io.train.fire
  val s0_vaddr        = io.train.bits.vaddr
  val s0_pc           = io.train.bits.pc
  val s0_pc_hash      = pc_hash_tag(s0_pc)
  val s0_pc_match_vec = VecInit(array zip valids map { case (e, v) => e.tag_match(v, s0_valid, s0_pc_hash) }).asUInt
  val s0_hit          = s0_pc_match_vec.orR
  val s0_index        = Mux(s0_hit, OHToUInt(s0_pc_match_vec), replacement.way)
//  io.train.ready             := s0_can_accept
//  io.stream_lookup_req.valid := s0_valid
//  io.stream_lookup_req.bits  := io.train_req.bits

  when(s0_valid) {
    replacement.access(s0_index)
  }

  // s1: alloc or update
  val s1_valid       = RegNext(s0_valid)
  val s1_index       = RegEnable(s0_index, s0_valid)
  val s1_pc_hash     = RegEnable(s0_pc_hash, s0_valid)
  val s1_vaddr       = RegEnable(s0_vaddr, s0_valid)
  val s1_hit         = RegEnable(s0_hit, s0_valid)
  val s1_alloc       = s1_valid && !s1_hit
  val s1_update      = s1_valid && s1_hit
  val s1_stride      = array(s1_index).stride
  val s1_new_stride  = WireInit(0.U(log2Up(strideMax).W))
  val s1_can_send_pf = WireInit(false.B)
  s0_can_accept := !(s1_valid && s1_pc_hash === s0_pc_hash)

  val always_update = true.B

  when(s1_alloc) {
    valids(s1_index) := true.B
    array(s1_index).alloc(
      vaddr = s1_vaddr,
      alloc_hash_pc = s1_pc_hash,
    )
  }.elsewhen(s1_update) {
    val res = array(s1_index).update(s1_vaddr, always_update)
    s1_can_send_pf := res._1
    s1_new_stride  := res._2
  }

  val l1_stride_ratio_const = 2.U
  val l1_stride_ratio       = l1_stride_ratio_const(3, 0)
  // s2: calculate L1 & L2 pf addr
  val s2_valid       = RegNext(s1_valid && s1_can_send_pf)
  val s2_vaddr       = RegEnable(s1_vaddr, s1_valid && s1_can_send_pf)
  val s2_stride      = RegEnable(s1_stride, s1_valid && s1_can_send_pf)
  val s2_l1_depth    = s2_stride << l1_stride_ratio
  val s2_l1_pf_vaddr = (s2_vaddr + s2_l1_depth)(vaddrWidth - 1, 0)

  val s2_l1_pf_req_bits = Wire(new PrefetchRequest())

  s2_l1_pf_req_bits.vaddr          := s2_l1_pf_vaddr
  s2_l1_pf_req_bits.write          := DontCare
  s2_l1_pf_req_bits.prefetchSource := PrefetchSource.Stride

//  val s2_l1_pf_req_bits = (new StreamPrefetchReqBundle).getStreamPrefetchReqBundle(
//    valid = s2_valid,
//    vaddr = s2_l1_pf_vaddr,
//    width = STRIDE_WIDTH_BLOCKS,
//    decr_mode = false.B,
//    sink = SINK_L1,
//    source = L1_HW_PREFETCH_STRIDE,
//    t_pc = 0xdeadbeefL.U,
//    t_va = 0xdeadbeefL.U,
//  )

  // s3: send l1 pf out
  val s3_valid          = RegNext(s2_valid)
  val s3_l1_pf_req_bits = RegEnable(s2_l1_pf_req_bits, s2_valid)

  io.req.valid := s3_valid
  io.req.bits  := s3_l1_pf_req_bits

//  for (i <- 0 until STRIDE_ENTRY_NUM) {
//    when(GatedValidRegNext(io.flush)) {
//      reset_array(i)
//    }
//  }

}
