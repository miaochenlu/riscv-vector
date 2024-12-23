package grapecoveDCache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import _root_.circt.stage.ChiselStage
//import grapecoveDCache._

class MSHRWrapper(
    implicit edge: TLEdgeOut
) extends Module {
  val io = IO(new Bundle {
    val req = Flipped(DecoupledIO(new MSHRWrapperPipeReq(edge.bundle)))

    val l2Req      = DecoupledIO(new TLBundleA(edge.bundle))
    val fromRefill = Flipped(DecoupledIO(new RefillMSHRFile()))

    val nextCycleWb          = Output(Bool())
    val nextCycleWb_sourceId = Output(UInt(MasterSource.width.W))
    val resp                 = ValidIO(new DataExchangeResp)

    val probeCheck    = new ProbeMSHRFile
    val probeRefill   = ValidIO(new ProbeRefill)
    val toReplace     = DecoupledIO(new MSHRReplace())
    val replaceStatus = Input(ReplaceStatus())

    val fenceRdy = Output(Bool())
  })

  val mshrs   = Module(new MSHRFile())
  val iomshrs = Module(new IOMSHRFile())

  val validIOMSHRReq = (!io.req.bits.cacheable || io.req.bits.noAlloc) &&
    !mshrs.io.addrMatch &&
    !iomshrs.io.addrMatch
  val validMSHRReq = Mux(
    !io.req.bits.cacheable || iomshrs.io.addrMatch,
    false.B,
    Mux(io.req.bits.noAlloc, mshrs.io.addrMatch, true.B),
  )

  // req signal connect
  mshrs.io.pipelineReq.valid              := io.req.valid && validMSHRReq
  mshrs.io.pipelineReq.bits.isUpgrade     := io.req.bits.isUpgrade
  mshrs.io.pipelineReq.bits.data          := io.req.bits.wdata
  mshrs.io.pipelineReq.bits.amoData       := io.req.bits.amoData
  mshrs.io.pipelineReq.bits.mask          := io.req.bits.wmask
  mshrs.io.pipelineReq.bits.lineAddr      := AddrDecoder.getLineAddr(io.req.bits.paddr)
  mshrs.io.pipelineReq.bits.meta.sourceId := io.req.bits.source
  mshrs.io.pipelineReq.bits.meta.offset   := AddrDecoder.getBlockOffset(io.req.bits.paddr)
  mshrs.io.pipelineReq.bits.meta.cmd      := io.req.bits.cmd
  mshrs.io.pipelineReq.bits.meta.regIdx   := io.req.bits.dest
  mshrs.io.pipelineReq.bits.meta.size     := io.req.bits.size
  mshrs.io.pipelineReq.bits.meta.signed   := io.req.bits.signed

  iomshrs.io.req.valid := io.req.valid && validIOMSHRReq
  iomshrs.io.req.bits  := io.req.bits

  io.req.ready := Mux(
    validIOMSHRReq,
    iomshrs.io.req.ready,
    Mux(validMSHRReq, mshrs.io.pipelineReq.ready, false.B),
  )

  // request L2 using TL A
  val acquire = edge.AcquireBlock(
    fromSource = mshrs.io.toL2Req.bits.entryId,
    toAddress = mshrs.io.toL2Req.bits.lineAddr << blockOffBits,
    lgSize = log2Ceil(blockBytes).U,
    growPermissions = mshrs.io.toL2Req.bits.perm,
  )._2

  io.l2Req.valid         := mshrs.io.toL2Req.valid || iomshrs.io.l2Req.valid
  mshrs.io.toL2Req.ready := io.l2Req.ready && !iomshrs.io.l2Req.valid
  iomshrs.io.l2Req.ready := io.l2Req.ready
  io.l2Req.bits          := Mux(iomshrs.io.l2Req.valid, iomshrs.io.l2Req.bits, acquire)

  // refill data
  val refillMSHR = io.fromRefill.bits.entryId < nMSHRs.asUInt
  val refillIOMSHR = io.fromRefill.bits.entryId >= (nMSHRs + nWBQEntries).asUInt &&
    io.fromRefill.bits.entryId < (nMSHRs + nWBQEntries + nMMIOs).asUInt

//  assert(!refillMSHR && !refillIOMSHR && io.fromRefill.valid)

  mshrs.io.fromRefill.valid := io.fromRefill.valid && refillMSHR
  mshrs.io.fromRefill.bits  := io.fromRefill.bits

  iomshrs.io.fromRefill.valid := io.fromRefill.valid && refillIOMSHR // && io.fromRefill.bits.hasData
  iomshrs.io.fromRefill.bits  := io.fromRefill.bits

  io.fromRefill.ready := Mux(
    refillMSHR,
    mshrs.io.fromRefill.ready,
    Mux(refillIOMSHR, iomshrs.io.fromRefill.ready, false.B),
  )

  // load resp to cpu
  val mshrsResp = Wire(new DataExchangeResp)
  mshrsResp.status  := CacheRespStatus.refill
  mshrsResp.source  := mshrs.io.toPipeline.bits.sourceId
  mshrsResp.dest    := mshrs.io.toPipeline.bits.regIdx
  mshrsResp.data    := mshrs.io.toPipeline.bits.regData
  mshrsResp.size    := mshrs.io.toPipeline.bits.size
  mshrsResp.hasData := mshrs.io.toPipeline.valid

  io.nextCycleWb := mshrs.io.toPipeline.bits.nextCycleWb || iomshrs.io.nextCycleWb
  io.nextCycleWb_sourceId := Mux(
    mshrs.io.toPipeline.bits.nextCycleWb,
    mshrs.io.toPipeline.bits.nextCycleWb_sourceId,
    iomshrs.io.nextCycleWb_sourceId,
  )
  dontTouch(io.nextCycleWb_sourceId)
  io.resp.valid         := mshrs.io.toPipeline.valid || iomshrs.io.resp.valid
  io.resp.bits          := Mux(mshrs.io.toPipeline.valid, mshrsResp, iomshrs.io.resp.bits)
  iomshrs.io.resp.ready := !mshrs.io.toPipeline.valid

  // others for MSHR
  io.toReplace <> mshrs.io.toReplace
  io.probeRefill <> mshrs.io.probeRefill
  io.probeCheck <> mshrs.io.probeCheck
  mshrs.io.replaceStatus := io.replaceStatus

  io.fenceRdy := mshrs.io.fenceRdy && iomshrs.io.fenceRdy
}
