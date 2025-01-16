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

trait DCacheAMOTestTrait {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

  val cacheReqDefault = CacheReqBundle()

  implicit val valName = ValName("DCacheAMOTest")

  def cacheTest0(): Unit =
    it should "pass: amoswap hit" in {
      test(LazyModule(new DCacheWrapper(true)(Parameters.empty)).module).withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        DCacheInit.initDut(dut)

        val cacheReq = CacheReqBundle(
          paddr = "h80004008",
          size = 3,
        )

        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReq.copy(wdata = "h7890", cmd = M_XA_SWAP)))

        dut.clock.step(1)

        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReq.copy(cmd = M_XRD)))

        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect("h1010101010101010".U)
        dut.io.resp.bits.status.expect(CacheRespStatus.hit)

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect(0x7890.U)
        dut.io.resp.bits.status.expect(CacheRespStatus.hit)
        dut.clock.step(10)
      }
    }

  def cacheTest1(): Unit =
    it should "pass: amoswap miss" in {
      test(LazyModule(new DCacheWrapper(true)(Parameters.empty)).module).withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        DCacheInit.initDut(dut)

        // evict 80004000

        val cacheReadReq = CacheReqBundle(
          paddr = "h8000c000",
          cmd = M_XRD,
          dest = 16,
        )

        // req miss
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReadReq))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.miss)
        dut.clock.step(1)

        dut.clock.step(200) // writeback

        val cacheReq = CacheReqBundle(
          paddr = "h80004008",
          size = 2,
          signed = true,
        )
        // swap
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReq.copy(wdata = "h7890", cmd = M_XA_SWAP)))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.miss)

        // swap refill
        dut.clock.step(1)
        while (!dut.io.resp.valid.peekBoolean()) {
          dut.clock.step(1)
        }
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect("h10101010".U)
        dut.io.resp.bits.status.expect(CacheRespStatus.refill)

        dut.clock.step(50)
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReq.copy(cmd = M_XRD)))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.hit)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect(0x7890.U)
        dut.clock.step(10)
      }
    }

  def cacheTest2(): Unit =
    it should "pass: amoadd hit" in {
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

        dut.clock.step(1)

        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReq.copy(cmd = M_XRD)))

        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect("h2323232323232323".U)
        dut.io.resp.bits.status.expect(CacheRespStatus.hit)

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect("h2424242424242424".U)
        dut.io.resp.bits.status.expect(CacheRespStatus.hit)
        dut.clock.step(10)
      }
    }

  def cacheTest3(): Unit =
    it should "pass: amoadd no perm miss" in {
      test(LazyModule(new DCacheWrapper(true)(Parameters.empty)).module).withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        DCacheInit.initDut(dut)

        val cacheReq = CacheReqBundle(
          paddr = "h8000a000",
          size = 3,
        )

        // first -> no perm miss
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReq.copy(
          wdata = "h0101010101010101",
          cmd = M_XA_ADD,
        )))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.miss)

        dut.clock.step(20)

        // second req hit
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReq.copy(
          wdata = "h0101010101010101",
          cmd = M_XA_ADD,
        )))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.hit)
        dut.io.resp.bits.data.expect("h2424242424242424".U)

        dut.clock.step(10)
      }
    }

  def cacheTest4(): Unit =
    it should "pass: load miss -> amoswap miss -> load miss" in {
      test(LazyModule(new DCacheWrapper(true)(Parameters.empty)).module).withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        DCacheInit.initDut(dut)

        // evict 80004000

        val cacheReadReq = CacheReqBundle(
          paddr = "h8000c000",
          cmd = M_XRD,
          dest = 16,
        )

        // req miss
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReadReq))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.miss)
        dut.clock.step(1)

        dut.clock.step(200) // writeback

        val cacheReq = CacheReqBundle(
          paddr = "h80004008",
          size = 2,
          signed = true,
        )
        // load miss
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReq.copy(cmd = M_XRD)))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.miss)

        // swap
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReq.copy(wdata = "h7890", cmd = M_XA_SWAP)))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.miss)

        // load miss
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReq.copy(paddr = "h80004000", size = 6, cmd = M_XRD)))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.miss)

        dut.clock.step(1)

        while (!dut.io.resp.valid.peekBoolean()) {
          dut.clock.step(1)
        }
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect("h10101010".U)
        dut.io.resp.bits.status.expect(CacheRespStatus.refill)
        dut.clock.step(1)

        while (!dut.io.resp.valid.peekBoolean()) {
          dut.clock.step(1)
        }
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect("h10101010".U)
        dut.io.resp.bits.status.expect(CacheRespStatus.refill)
        dut.clock.step(1)

        while (!dut.io.resp.valid.peekBoolean()) {
          dut.clock.step(1)
        }
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect(
          "h0123456789abcdef_fedcba9876543210_0011223344556677_8899aabbccddeeff_7766554433221100_ffeeddccbbaa9988_1010101000007890_2323232323232323".U
        )
        dut.io.resp.bits.status.expect(CacheRespStatus.refill)
        dut.clock.step(1)
      }
    }

}

class DCacheAMOTest extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with DCacheAMOTestTrait {
  behavior of "DCache AMO Test"

  it should behave like cacheTest0() //
  it should behave like cacheTest1() //
  it should behave like cacheTest2() //
  it should behave like cacheTest3() //
  it should behave like cacheTest4() //
}
