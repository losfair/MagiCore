package magicore.isa.riscv

import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._

import magicore.control._
import magicore.frontend._
import org.scalatest.funsuite.AnyFunSuite
import magicore.util.PolymorphicDataChain
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import magicore.testutil.TestExt._
import scala.collection.mutable
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.misc.SizeMapping
import magicore.testutil.TestSyncResetSpinalConfig

class TestRiscvDecompressor extends AnyFunSuite {
  case class MinFetchPacket() extends Bundle {
    val pc = UInt(32 bits)
    val insn = Bits(32 bits)

    def toFetchPacket(): FetchPacket = {
      val pkt = FetchPacket()
      pkt.cacheMiss := False
      pkt.globalHistory := 0
      pkt.insn := insn
      pkt.pc := pc
      pkt.pcTag := False
      pkt.predictedBranchTarget := 0
      pkt.predictedBranchValid := False
      pkt
    }
  }

  case class SimInsn(pc: BigInt, insn: BigInt, decompressed: BigInt, len: Int)

  private def genInsnSeq(len: Int): Seq[SimInsn] = {
    var pc = BigInt(0)
    def genRegular(x: BigInt): SimInsn = {
      val out = SimInsn(pc = pc, insn = x, decompressed = x, len = 4)
      pc += 4
      out
    }
    def genComp(x: BigInt, decompressed: BigInt): SimInsn = {
      val out = SimInsn(pc = pc, insn = x, decompressed = decompressed, len = 2)
      pc += 2
      out
    }
    (0 until len).map { _ =>
      val rand = Random.nextInt(20)
      rand match {
        case x if 0 until 10 contains x =>
          genComp(
            0x4090,
            0x0004a603
          ) // 90 40                        	lw	a2, 0(s1)
        case x if 10 until 20 contains x =>
          genRegular(0x00060b1b) // 1b 0b 06 00                  	sext.w	s6, a2
      }
    }
  }

  case class SimFetchPayload(pc: BigInt, insn: BigInt)

  private def toFetchPayload(
      from: Seq[SimInsn],
      initialByteOffset: Int
  ): Seq[SimFetchPayload] = {
    val bytes = new ArrayBuffer[Short]()
    for (_ <- 0 until initialByteOffset) {
      bytes += 0
    }
    for (insn <- from) {
      for (i <- 0 until insn.len) {
        bytes += ((insn.insn >> (i * 8)) & 0xff).toShort
      }
    }
    val result = bytes
      .grouped(4)
      .map(g =>
        BigInt(
          g.map(x => x.toByte)
            .padTo(4, 0xff.toByte)
            .padTo(5, 0.toByte)
            .reverse
            .to[Array]
        )
      )
      .zipWithIndex
      .map { case (x, i) => SimFetchPayload(i * 4, x) }
      .to[ArrayBuffer]
    result.update(0, result(0).copy(pc = initialByteOffset))
    result.toSeq
  }

  case class TestRiscvDecompressorTop() extends Component {
    val mspec = MachineSpec(
      numArchitecturalRegs = 32,
      numPhysicalRegs = 64,
      addrWidth = 32 bits,
      dataWidth = 32 bits,
      maxNumSrcRegsPerInsn = 2,
      maxNumDstRegsPerInsn = 1,
      issueQueueSize = 16,
      functionUnitTagType = HardType(UInt(1 bits)),
      robSize = 64,
      writebackWidth = 2
    )

    val fspec = FrontendSpec(
      icacheSize = 16384,
      icacheMemPortDataWidth = 32,
      insnWidth = 32 bits,
      addrWidth = 32 bits,
      resetPc = 0,
      globalHistorySize = 4096,
      btbSize = 128,
      initBranchPredictionBuffers = false
    )
    Machine.provide(mspec)
    Machine.provide(fspec)
    val exc = MachineException()
    exc.assignDontCare()
    Machine.provide(exc)
    val decompressor = RiscvDecompressor()

    val io = new Bundle {
      val input = slave(Stream(MinFetchPacket()))
      val output = master(Stream(MinFetchPacket()))
      val excValid = in(Bool())
    }

    io.input.translateWith(
      io.input.payload.toFetchPacket()
    ) >> decompressor.input

    val output = MinFetchPacket()
    output.pc := decompressor.output.pc
    output.insn := decompressor.output.insn
    decompressor.output.translateWith(output) >> io.output

    exc.valid := io.excValid
  }

  test("TestRiscvDecompressor") {
    SimConfig
      .withConfig(TestSyncResetSpinalConfig)
      .withWave
      .doSim(
        rtl = Machine.build { new TestRiscvDecompressorTop() },
        name = "test"
      ) { dut =>
        dut.io.input.valid #= false
        dut.io.excValid #= false

        dut.clockDomain.forkStimulus(100)
        dut.clockDomain.waitSamplingWhere(dut.clockDomain.isResetAsserted)
        dut.clockDomain.waitSamplingWhere(dut.clockDomain.isResetDeasserted)

        fork {
          while (true) {
            dut.clockDomain.waitSampling()
            dut.io.output.ready #= Random.nextBoolean()
          }
        }

        val numIterations = 100

        for (i <- 0 until numIterations) {
          dut.io.excValid #= true
          dut.clockDomain.waitSampling()
          dut.io.excValid #= false
          dut.clockDomain.waitSampling()

          var insnSeq = genInsnSeq(1000)
          val byteOffset = Random.nextInt(2) * 2
          val fetchPayload = toFetchPayload(insnSeq, byteOffset)
          var readPointer = 0

          val checkerTask = fork {
            while (true) {
              dut.clockDomain.waitSampling()
              if (
                dut.io.output.valid.toBoolean && dut.io.output.ready.toBoolean
              ) {
                val expected = insnSeq(readPointer)
                val expectedPC = expected.pc + byteOffset
                val actualPC = dut.io.output.payload.pc.toBigInt
                val actualInsn = dut.io.output.payload.insn.toBigInt
                if (
                  expectedPC != actualPC || expected.decompressed != actualInsn
                ) {
                  throw new Exception(
                    s"At read pointer ${readPointer}: Expected: ${expectedPC} ${expected.decompressed} Actual: ${actualPC} ${actualInsn}"
                  )
                }
                readPointer += 1
              }
            }
          }

          for (item <- fetchPayload) {
            dut.io.input.simWrite(
              dut,
              p => {
                p.pc #= item.pc
                p.insn #= item.insn
              }
            )
          }

          while (readPointer < insnSeq.size) {
            dut.clockDomain.waitSampling()
          }

          for(_ <- 0 until 10) {
            dut.clockDomain.waitSampling()
            assert(readPointer == insnSeq.size)
          }
          checkerTask.terminate()
        }
        println(s"Check passed. iterations=${numIterations}")
      }
  }
}
