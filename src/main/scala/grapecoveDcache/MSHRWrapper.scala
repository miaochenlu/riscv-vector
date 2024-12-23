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

    val nextCycleWb  = Output(Bool())
    val nextSourceId = Output(UInt(MasterSource.width.W))
    val fenceRdy     = Output(Bool())
    val resp         = ValidIO(new DataExchangeResp)

    val l2Req      = DecoupledIO(new TLBundleA(edge.bundle))
    val fromRefill = Flipped(DecoupledIO(new RefillMSHRFile()))

    val probeCheck    = new ProbeMSHRFile
    val probeRefill   = ValidIO(new ProbeRefill)
    val toReplace     = DecoupledIO(new MSHRReplace())
    val replaceStatus = Input(ReplaceStatus())
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

  io.req.ready := MuxCase(
    false.B,
    Array(
      validIOMSHRReq -> iomshrs.io.req.ready,
      validMSHRReq   -> mshrs.io.pipelineReq.ready,
    ),
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

  // request L2 using TL A with arbiter
  val arbiter = Module(new Arbiter(new TLBundleA(edge.bundle), 2))

  // source 0: iomshr
  arbiter.io.in(0) <> iomshrs.io.l2Req
  // source 1: mshr
  arbiter.io.in(1).valid := mshrs.io.toL2Req.valid
  mshrs.io.toL2Req.ready := arbiter.io.in(1).ready
  arbiter.io.in(1).bits := edge.AcquireBlock(
    fromSource = mshrs.io.toL2Req.bits.entryId,
    toAddress = mshrs.io.toL2Req.bits.lineAddr << blockOffBits,
    lgSize = log2Ceil(blockBytes).U,
    growPermissions = mshrs.io.toL2Req.bits.perm,
  )._2

  io.l2Req <> arbiter.io.out

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

  val respArbiter = Module(new Arbiter(new DataExchangeResp, 2))
  respArbiter.io.in(0).valid := mshrs.io.toPipeline.valid
  respArbiter.io.in(0).bits  := mshrsResp
  respArbiter.io.in(1) <> iomshrs.io.resp
  respArbiter.io.out.ready := true.B

  io.resp.valid := respArbiter.io.out.valid
  io.resp.bits  := respArbiter.io.out.bits

  io.nextCycleWb := mshrs.io.toPipeline.bits.nextCycleWb || iomshrs.io.nextCycleWb
  io.nextSourceId := Mux(
    mshrs.io.toPipeline.bits.nextCycleWb,
    mshrs.io.toPipeline.bits.nextSourceId,
    iomshrs.io.nextSourceId,
  )

  // others for MSHR
  io.toReplace <> mshrs.io.toReplace
  io.probeRefill <> mshrs.io.probeRefill
  io.probeCheck <> mshrs.io.probeCheck
  mshrs.io.replaceStatus := io.replaceStatus

  io.fenceRdy := mshrs.io.fenceRdy && iomshrs.io.fenceRdy
}
