package grapecoveDcache
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.tilelink._
import grapecoveDCache.AddrDecoder.getLineAddr
import grapecoveDCache._
import grapecoveDCache.PrefetcherHashFunc._

case class StreamPrefetcherParams(
) extends CanInstantiatePrefetcher {
  def desc() = "Stream Prefetcher"
  def instantiate()(
      implicit p: Parameters
  ) = Module(new StreamPrefetcher(this)(p))
}

class StreamBitVectorBundle(
    implicit p: Parameters
) extends Bundle with DCacheParams {
  val tag     = UInt(streamTagWidth.W)
  val bit_vec = UInt(streamBitVecWidth.W)
  val active  = Bool()

  val cnt = UInt((log2Up(streamBitVecWidth) + 1).W)
//  val decr_mode = Bool()

  def tag_match(valid1: Bool, valid2: Bool, new_tag: UInt): Bool =
    valid1 && valid2 && region_hash_tag(tag) === region_hash_tag(new_tag)

  def alloc(alloc_tag: UInt, alloc_bit_vec: UInt, alloc_active: Bool, alloc_decr_mode: Bool, alloc_full_vaddr: UInt) = {
    tag     := alloc_tag
    bit_vec := alloc_bit_vec
    active  := alloc_active
    cnt     := 1.U
  }

  def update(update_bit_vec: UInt, update_active: Bool) = {
    // if the slot is 0 before, increment cnt
    val cnt_en   = !(bit_vec & update_bit_vec).orR
    val cnt_next = Mux(cnt_en, cnt + 1.U, cnt)

    bit_vec := bit_vec | update_bit_vec
    cnt     := cnt_next
    when(cnt_next >= streamConfTH.U) {
      active := true.B
    }
    when(update_active) {
      active := true.B
    }
  }
}

class StreamPrefetcher(
    params: StreamPrefetcherParams
)(
    implicit p: Parameters
) extends AbstractPrefetcher()(p) {
  val desc = "StreamPrefetcher"

  val array  = Reg(Vec(streamEntryNum, new StreamBitVectorBundle))
  val valids = RegInit(VecInit(Seq.fill(streamEntryNum)(false.B)))

  val replacement = ReplacementPolicy.fromString("plru", streamEntryNum)

  // s0: generate region tag, parallel match
//  val s0_can_accept          = Wire(Bool())
  val s0_valid               = io.train.fire
  val s0_pc                  = io.train.bits.pc
  val s0_vaddr               = io.train.bits.vaddr
  val s0_region_bits         = get_region_bits(s0_vaddr)
  val s0_region_tag          = get_region_tag(s0_vaddr)
  val s0_region_tag_plus_one = get_region_tag(s0_vaddr) + 1.U
  val s0_region_tag_match_vec =
    array zip valids map { case (e, v) => e.tag_match(v, s0_valid, s0_region_tag) }
  val s0_region_tag_plus_one_match_vec =
    array zip valids map { case (e, v) =>
      e.tag_match(v, s0_valid, s0_region_tag_plus_one)
    }
  val s0_hit            = Cat(s0_region_tag_match_vec).orR
  val s0_plus_one_hit   = Cat(s0_region_tag_plus_one_match_vec).orR
  val s0_hit_vec        = VecInit(s0_region_tag_match_vec).asUInt
  val s0_index          = Mux(s0_hit, OHToUInt(s0_hit_vec), replacement.way)
  val s0_plus_one_index = OHToUInt(VecInit(s0_region_tag_plus_one_match_vec).asUInt)

  when(s0_valid) {
    replacement.access(s0_index)
  }

  // s1: alloc or update
  val s1_valid = RegNext(s0_valid)
  val s1_index = RegEnable(s0_index, s0_valid)
//  val s1_pc             = RegEnable(s0_pc, s0_valid)
  val s1_vaddr          = RegEnable(s0_vaddr, s0_valid)
  val s1_plus_one_index = RegEnable(s0_plus_one_index, s0_valid)
  val s1_hit            = RegEnable(s0_hit, s0_valid)
  val s1_plus_one_hit = if (ENABLE_STRICT_ACTIVE_DETECTION)
    RegEnable(s0_plus_one_hit, s0_valid) && array(s1_plus_one_index).active &&
    (array(s1_plus_one_index).cnt >= streamConfTH.U)
  else
    RegEnable(s0_plus_one_hit, s0_valid) && array(s1_plus_one_index).active
  val s1_region_tag  = RegEnable(s0_region_tag, s0_valid)
  val s1_region_bits = RegEnable(s0_region_bits, s0_valid)
  val s1_alloc       = s1_valid && !s1_hit
  val s1_update      = s1_valid && s1_hit
  val s1_pf_l1_incr_vaddr =
    Cat(Cat(s1_region_tag, s1_region_bits) + streamDepth.U, 0.U(log2Up(blockBytes).W))
  val s1_can_send_pf = Mux(s1_update, !(array(s1_index).bit_vec & UIntToOH(s1_region_bits)).orR, true.B)
//  s0_can_accept := !(s1_valid && (region_hash_tag(s1_region_tag) === region_hash_tag(s0_region_tag)))

  when(s1_alloc) {
    // alloc a new entry
    valids(s1_index) := true.B
    array(s1_index).alloc(
      alloc_tag = s1_region_tag,
      alloc_bit_vec = UIntToOH(s1_region_bits),
      alloc_active = s1_plus_one_hit,
      alloc_decr_mode = RegEnable(s0_plus_one_hit, s0_valid),
      alloc_full_vaddr = RegEnable(s0_vaddr, s0_valid),
    )

  }.elsewhen(s1_update) {
    // update a existing entry
    assert(array(s1_index).cnt =/= 0.U || valids(s1_index), "entry should have been allocated before")
    array(s1_index).update(
      update_bit_vec = UIntToOH(s1_region_bits),
      update_active = s1_plus_one_hit,
    )
  }

  // s2: trigger prefetch if hit active bit vector, compute meta of prefetch req
  val s2_valid = RegNext(s1_valid)
  val s2_index = RegEnable(s1_index, s1_valid)
//  val s2_pc               = RegEnable(s1_pc, s1_valid)
//  val s2_vaddr = RegEnable(s1_vaddr, s1_valid)
//  val s2_region_bits      = RegEnable(s1_region_bits, s1_valid)
//  val s2_region_tag       = RegEnable(s1_region_tag, s1_valid)
  val s2_pf_l1_incr_vaddr = RegEnable(s1_pf_l1_incr_vaddr, s1_valid)
  val s2_can_send_pf      = RegEnable(s1_can_send_pf, s1_valid)
  val s2_active           = array(s2_index).active
  val s2_l1_vaddr         = s2_pf_l1_incr_vaddr
  val s2_will_send_pf     = s2_valid && s2_active && s2_can_send_pf
  val s2_pf_req_valid     = s2_will_send_pf // && io.enable
  val s2_pf_l1_req_bits   = Wire(new PrefetchRequest())
  s2_pf_l1_req_bits.vaddr          := s2_l1_vaddr
  s2_pf_l1_req_bits.write          := DontCare // FIXME
  s2_pf_l1_req_bits.prefetchSource := PrefetchSource.Stream

  // s3: send the l1 prefetch req out
  val s3_pf_l1_valid = RegNext(s2_pf_req_valid)
  val s3_pf_l1_bits  = RegEnable(s2_pf_l1_req_bits, s2_pf_req_valid)

  io.req.valid      := s3_pf_l1_valid
  io.req.bits.vaddr := s3_pf_l1_bits

  // Stride lookup starts here
  // S0: Stride send req
//  val s0_lookup_valid = io.stream_lookup_req.valid
//  val s0_lookup_vaddr = io.stream_lookup_req.bits.vaddr
//  val s0_lookup_tag   = get_region_tag(s0_lookup_vaddr)
//  // S1: match
//  val s1_lookup_valid         = RegNext(s0_lookup_valid)
//  val s1_lookup_tag           = RegEnable(s0_lookup_tag, s0_lookup_valid)
//  val s1_lookup_tag_match_vec = array zip valids map { case (e, v) => e.tag_match(v, s1_lookup_valid, s1_lookup_tag) }
//  val s1_lookup_hit           = VecInit(s1_lookup_tag_match_vec).asUInt.orR
//  val s1_lookup_index         = OHToUInt(VecInit(s1_lookup_tag_match_vec))
//  // S2: read active out
//  val s2_lookup_valid  = GatedValidRegNext(s1_lookup_valid)
//  val s2_lookup_hit    = RegEnable(s1_lookup_hit, s1_lookup_valid)
//  val s2_lookup_index  = RegEnable(s1_lookup_index, s1_lookup_valid)
//  val s2_lookup_active = array(s2_lookup_index).active
//  // S3: send back to Stride
//  val s3_lookup_valid  = GatedValidRegNext(s2_lookup_valid)
//  val s3_lookup_hit    = RegEnable(s2_lookup_hit, s2_lookup_valid)
//  val s3_lookup_active = RegEnable(s2_lookup_active, s2_lookup_valid)
//  io.stream_lookup_resp := s3_lookup_valid && s3_lookup_hit && s3_lookup_active

  // reset meta to avoid muti-hit problem
//  for (i <- 0 until BIT_VEC_ARRAY_SIZE) {
//    when(GatedValidRegNext(io.flush)) {
//      reset_array(i)
//    }
//  }
}
