package grapecoveDCache
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink._
import _root_.circt.stage.ChiselStage

class DCacheWrapper(isTest: Boolean)(
    implicit p: Parameters
) extends LazyModule {

  val dcacheClient = LazyModule(new GPCDCache()(p))

  val mmio = LazyModule(new TLRAM(AddressSet(0x6000_0000L, 0x1fff_ffffL), beatBytes = beatBytes, atomics = true))
  val rams = if (isTest) { // use small range for local test
    Seq(LazyModule(new TLRAM(AddressSet(0x8000_0000L, 0x7fff_ffffL), beatBytes = beatBytes, atomics = true)))
  } else { // large range
    (1 until 7).map(i =>
      LazyModule(new TLRAM(AddressSet(i * 0x10_0000_0000L, 0xf_ffff_ffffL), beatBytes = beatBytes, atomics = true))
    ) ++ (1 until 15).map(i =>
      LazyModule(new TLRAM(AddressSet(i * 0x1_0000_0000L, 0xffff_ffffL), beatBytes = beatBytes, atomics = true))
    ) :+ LazyModule(new TLRAM(AddressSet(0x8000_0000L, 0x7fff_ffffL), beatBytes = beatBytes, atomics = true))
  }

  val xbar = TLXbar()
  mmio.node := xbar
  rams.foreach { ram =>
    ram.node :=*
      TLFragmenter(beatBytes, blockBytes) :=*
      TLCacheCork(unsafe = true) :=*
      TLDelayer(0) :=*
      xbar
  }

  xbar :=*
    dcacheClient.node

  lazy val module = new DCacheWrapperImp(this)
}

class DCacheWrapperImp(outer: DCacheWrapper) extends LazyModuleImp(outer) {
  val io = IO(new DataExchangeIO())
  outer.dcacheClient.module.io <> io
}

object Main extends App {

  val firtoolOptions = Array(
    "--lowering-options=" + List(
      "disallowLocalVariables",
      "disallowPortDeclSharing",
      "locationInfoStyle=wrapInAtSquareBracket",
    ).reduce(_ + "," + _),
    "--disable-annotation-unknown",
    "--disable-all-randomization",
  )

  lazy val dcacheWrapper = LazyModule(new DCacheWrapper(false)(Parameters.empty))
  ChiselStage.emitSystemVerilogFile(dcacheWrapper.dcacheClient.module, args, firtoolOptions)
  // ChiselStage.emitSystemVerilogFile(
  //   dcacheWrapper.dcacheClient.module,
  //   args ++ Array("--split-verilog"),
  //   firtoolOptions,
  // )
}
