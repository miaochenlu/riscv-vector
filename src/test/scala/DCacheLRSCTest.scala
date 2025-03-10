package grapecoveDCache

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.BundleLiterals._
import chiseltest.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import MemoryOpConstants._

trait DCacheLRSCTestTrait {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

  val cacheReqDefault = CacheReqBundle()

  implicit val valName = ValName("DCacheLRSCTest")

  def cacheTest0(): Unit =
    it should "pass: lrsc success" in {
      test(LazyModule(new DCacheWrapper(true)(Parameters.empty)).module).withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        DCacheInit.initDut(dut)

        val cacheReq = CacheReqBundle(
          paddr = "h80004000",
          size = 3,
        )

        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReq.copy(
          wdata = "h0101010101010101",
          cmd = M_XA_ADD,
        )))

        // XLR
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReq.copy(cmd = M_XLR)))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect("h2323232323232323".U)
        dut.io.resp.bits.status.expect(CacheRespStatus.hit)
        dut.clock.step(10)

        // XSC
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReq.copy(
          wdata = "h0101010101010101",
          cmd = M_XSC,
        )))
        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.hit)

        // read hit 64
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReq.copy(cmd = M_XRD)))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect("h101010101010101".U)
        dut.io.resp.bits.status.expect(CacheRespStatus.hit)
        dut.clock.step(10)
      }
    }

  def cacheTest1(): Unit =
    it should "pass: lr success sc fail" in {
      test(LazyModule(new DCacheWrapper(true)(Parameters.empty)).module).withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        DCacheInit.initDut(dut)

        val cacheReq = CacheReqBundle(
          paddr = "h80004000",
          size = 3,
        )

        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReq.copy(
          wdata = "h0101010101010101",
          cmd = M_XA_ADD,
        )))

        // XLR
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReq.copy(cmd = M_XLR)))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect("h2323232323232323".U)
        dut.io.resp.bits.status.expect(CacheRespStatus.hit)

        // time out
        dut.clock.step(80)

        // XSC
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReq.copy(
          wdata = "h0101010101010101",
          cmd = M_XSC,
        )))
        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect("h1".U)

        // read hit 64
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReq.copy(cmd = M_XRD)))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect("h2323232323232323".U)
        dut.io.resp.bits.status.expect(CacheRespStatus.hit)
        dut.clock.step(10)
      }
    }

  def cacheTest2(): Unit =
    it should "pass: lr fail->success sc success" in {
      test(LazyModule(new DCacheWrapper(true)(Parameters.empty)).module).withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        DCacheInit.initDut(dut)

        val cacheReq = CacheReqBundle(
          paddr = "h80003000",
          size = 3,
        )

        // XLR
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReq.copy(cmd = M_XLR)))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.replay)

        dut.clock.step(20)
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReq.copy(cmd = M_XLR)))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.hit)

        // XSC
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReq.copy(
          wdata = "h0101010101010101",
          cmd = M_XSC,
        )))
        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect("h0".U)

        // read hit 64
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReq.copy(cmd = M_XRD)))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect("h0101010101010101".U)
        dut.io.resp.bits.status.expect(CacheRespStatus.hit)
        dut.clock.step(10)
      }
    }

}

class DCacheLRSCTest extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with DCacheLRSCTestTrait {
  behavior of "DCache LRSC Test"

  it should behave like cacheTest0() //
  it should behave like cacheTest1() //
  it should behave like cacheTest2() //
}
