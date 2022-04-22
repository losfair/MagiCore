package miniooo.isa.riscv

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

class TestRiscv extends AnyFunSuite {
  val debug = false
  val memSize = 4096
  val numWords = memSize / 4

  case class RiscvProcessorTestTop() extends Component {
    val processorReset = Bool()
    val processor = new ResetArea(reset = processorReset, cumulative = true) {
      val v = RiscvProcessor(resetPc = 0x0, debug = debug)
    }.v

    val ocram = new Axi4SharedOnChipRam(
      dataWidth = 32,
      byteCount = memSize,
      idWidth = 16
    )

    val io = new Bundle {
      val probe = master(
        Axi4WriteOnly(processor.io.dBus.config.copy(idWidth = 16))
      )
      val mem = slave(
        Axi4WriteOnly(ocram.axiConfig.copy(useId = false, idWidth = 0))
      )
      val processorReset = in(Bool())
    }

    processorReset := io.processorReset

    Axi4CrossbarFactory()
      .addSlaves(
        ocram.io.axi -> SizeMapping(0, ocram.byteCount),
        io.probe -> SizeMapping(BigInt("fe000000", 16), 0x1000)
      )
      .addConnections(
        processor.io.dBus -> Seq(ocram.io.axi, io.probe),
        processor.io.iBus -> Seq(ocram.io.axi),
        io.mem -> Seq(ocram.io.axi)
      )
      .build()

    if (debug) {
      when(ocram.io.axi.arw.fire && !io.processorReset) {
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
      when(ocram.io.axi.r.fire && !io.processorReset) {
        Machine.report(
          Seq(
            "OCRAM read response: data=",
            ocram.io.axi.r.payload.data,
            " id=",
            ocram.io.axi.r.payload.id
          )
        )
      }
      when(processor.io.iBus.r.fire && !io.processorReset) {
        Machine.report(
          Seq(
            "iBus read fire: data=",
            processor.io.iBus.r.payload.data
          )
        )
      }
    }
  }

  test("TestRiscv") {
    SimConfig
      .withConfig(TestSyncResetSpinalConfig)
      .withWave
      .doSim(
        rtl = Machine.build { new RiscvProcessorTestTop() },
        name = "test"
      ) { dut =>
        dut.io.processorReset #= true
        dut.io.mem.aw.valid #= false
        dut.io.mem.w.valid #= false
        dut.io.mem.b.ready #= false

        dut.io.probe.aw.ready #= false
        dut.io.probe.w.ready #= false
        dut.io.probe.b.valid #= false

        dut.clockDomain.forkStimulus(100)
        waitUntil(dut.clockDomain.isResetAsserted)
        waitUntil(dut.clockDomain.isResetDeasserted)

        fork {
          for (i <- 0 until 1000000) {
            dut.clockDomain.waitSampling()
          }
          throw new Exception("Too many cycles")
        }

        val insnTemplate = Seq[Short](
          0x37, 0x11, 0x00, 0x00, 0x13, 0x05, 0xa0, 0x00, 0xef, 0x00, 0xc0,
          0x00, 0x13, 0x05, 0x05, 0x00, 0x6f, 0xf0, 0xdf, 0xff, 0x13, 0x01,
          0x01, 0xff, 0x23, 0x24, 0x81, 0x00, 0x23, 0x26, 0x11, 0x00, 0x13,
          0x04, 0xf5, 0xff, 0x23, 0x22, 0x91, 0x00, 0x23, 0x20, 0x21, 0x01,
          0x93, 0x07, 0x10, 0x00, 0x13, 0x05, 0x10, 0x00, 0x63, 0xf2, 0x87,
          0x02, 0x93, 0x04, 0x00, 0x00, 0x13, 0x09, 0x10, 0x00, 0x13, 0x05,
          0x04, 0x00, 0xef, 0xf0, 0x1f, 0xfd, 0x13, 0x04, 0xe4, 0xff, 0xb3,
          0x84, 0xa4, 0x00, 0xe3, 0x68, 0x89, 0xfe, 0x13, 0x85, 0x14, 0x00,
          0x83, 0x20, 0xc1, 0x00, 0x03, 0x24, 0x81, 0x00, 0x83, 0x24, 0x41,
          0x00, 0x03, 0x29, 0x01, 0x00, 0x13, 0x01, 0x01, 0x01, 0x67, 0x80,
          0x00, 0x00
        )

        // Zero out memory
        for (i <- 0 until numWords) {
          dut.io.mem.simWriteBytes(dut, i * 4, Seq(0, 0, 0, 0))
        }

        dut.io.mem.simWriteBytes(dut, 0, insnTemplate.map(_.toByte))

        println("Written instruction memory.")

        if (debug) fork {
          var cycles = 0
          while (true) {
            dut.clockDomain.waitFallingEdge()
            println("+++ cycle " + cycles + " +++")
            cycles += 1
          }
        }

        dut.io.processorReset #= false
        dut.clockDomain.waitSampling(20000)
      }
  }
}
