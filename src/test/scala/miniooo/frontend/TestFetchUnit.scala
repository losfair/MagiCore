package miniooo.frontend

import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._

import miniooo.control._
import miniooo.frontend._
import org.scalatest.funsuite.AnyFunSuite
import miniooo.util.PolymorphicDataChain
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import miniooo.testutil.TestExt._
import scala.collection.mutable
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.misc.SizeMapping
import miniooo.testutil.TestSyncResetSpinalConfig

class TestFetchUnit extends AnyFunSuite {
  val debug = true

  val mspec = MachineSpec(
    numArchitecturalRegs = 4,
    numPhysicalRegs = 64,
    addrWidth = 32 bits,
    dataWidth = 32 bits,
    maxNumSrcRegsPerInsn = 2,
    maxNumDstRegsPerInsn = 1,
    issueQueueSize = 32,
    functionUnitTagType = HardType(Bool()),
    robSize = 2,
    commitWidth = 1
  )
  val fspec = FrontendSpec(
    icacheSize = 4096,
    icacheMemPortDataWidth = 32,
    branchShiftCount = 2 bits
  )

  case class ExcCtx() extends Bundle with PolymorphicDataChain {
    val fetch = FetchPacket()
    val exc = MachineException()

    def parentObjects = Seq(fetch, exc)
  }

  val memSize = 1024

  class TestFetchUnitTop extends Component {
    if (debug) Machine.provide(MachineDebugMarker)
    Machine.provide(mspec)
    Machine.provide(fspec)

    val ocram = new Axi4SharedOnChipRam(
      dataWidth = fspec.insnWidth.value,
      byteCount = memSize,
      idWidth = 1
    )

    val io = new Bundle {
      val exc = in(FullMachineException(ExcCtx()))
      val mem = slave(
        Axi4WriteOnly(ocram.axiConfig.copy(useId = false, idWidth = 0))
      )
      val fetchOut = master(Stream(FetchPacket()))
      val fetchOut_payload_bits = out(Bits(FetchPacket().getBitsWidth bits))
      val exc_fetchPacket_bits = in(Bits(FetchPacket().getBitsWidth bits))
    }

    val exc = FullMachineException(ExcCtx())
    exc.ctx.lookup[FetchPacket] := io.exc_fetchPacket_bits.as(FetchPacket())
    exc.assignUnassignedByName(io.exc)
    Machine.provide(exc)
    Machine.provide(exc.exc)

    val fetch = FetchUnit()
    fetch.io.flush := False

    val brInfo = BranchInfoFeedback.idle
    fetch.io.branchInfoFeedback := brInfo
    io.fetchOut << fetch.io.output

    Axi4CrossbarFactory()
      .addSlave(ocram.io.axi, SizeMapping(0, ocram.byteCount))
      .addConnections(
        fetch.io.memBus -> Seq(ocram.io.axi),
        io.mem -> Seq(ocram.io.axi)
      )
      .build()

    io.fetchOut_payload_bits := io.fetchOut.payload.asBits

    when(ocram.io.axi.arw.fire) {
      Machine.report(
        Seq(
          "OCRAM request: addr=",
          ocram.io.axi.arw.addr,
          " size=",
          ocram.io.axi.arw.size,
          " len=",
          ocram.io.axi.arw.len,
          " write=",
          ocram.io.axi.arw.write
        )
      )
    }

  }
  test("TestFetchUnit") {
    SimConfig
      .withConfig(TestSyncResetSpinalConfig)
      .withWave
      .doSim(
        rtl = Machine.build { new TestFetchUnitTop() },
        name = "test",
        seed = 379034447
      ) { dut =>
        dut.io.exc.exc.valid #= false
        dut.io.mem.aw.valid #= false
        dut.io.mem.w.valid #= false
        dut.io.mem.b.ready #= false
        dut.io.fetchOut.ready #= false

        dut.clockDomain.forkStimulus(100)
        waitUntil(dut.clockDomain.isResetAsserted)
        waitUntil(dut.clockDomain.isResetDeasserted)

        fork {
          for (i <- 0 until 100000) {
            dut.clockDomain.waitSampling()
          }
          throw new Exception("Too many cycles")
        }

        if (debug) fork {
          var cycles = 0
          while (true) {
            dut.clockDomain.waitFallingEdge()
            println("+++ cycle " + cycles + " +++")
            cycles += 1
          }
        }

        val insnMem = (0 until memSize / 4).map(i => {
          BigInt(i)
        })

        // Write
        for ((word, i) <- insnMem.zipWithIndex) {
          val bytes = word.toByteArray.reverse.padTo(4, 0.toByte)
          dut.io.mem.simWriteBytes(dut, i * bytes.size, bytes)
        }
        println("Initialized instruction memory.")

        dut.io.fetchOut.ready #= true

        // Wait for the first cache miss
        dut.clockDomain.waitSamplingWhere(dut.io.fetchOut.valid.toBoolean)
        assert(dut.io.fetchOut.payload.cacheMiss.toBoolean)
        assert(dut.io.fetchOut.payload.pc.toBigInt == 0)
        val fp = dut.io.fetchOut_payload_bits.toBigInt

        // Trigger the exception
        dut.clockDomain.waitSampling(Random.nextInt(10) + 1)
        dut.io.exc.exc.valid #= true
        dut.io.exc.exc.code #= MachineExceptionCode.INSN_CACHE_MISS
        dut.io.exc_fetchPacket_bits #= fp
        dut.clockDomain.waitSampling()
        dut.io.exc.exc.valid #= false

        dut.clockDomain.waitSamplingWhere(!dut.io.fetchOut.valid.toBoolean)
        dut.clockDomain.waitSamplingWhere(dut.io.fetchOut.valid.toBoolean)
        assert(!dut.io.fetchOut.payload.cacheMiss.toBoolean)
        assert(dut.io.fetchOut.payload.pc.toBigInt == 0)

        println("OK")
        dut.clockDomain.waitSampling(5)
      }
  }
}
