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
  val debug = false

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
    globalHistorySize = 2048
  )

  case class ExcCtx() extends Bundle with PolymorphicDataChain {
    val fetch = FetchPacket()
    val exc = MachineException()

    def parentObjects = Seq(fetch, exc)
  }

  val memSize = 4096

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

    val brInfo = BranchInfoFeedback()
    brInfo.isUnconditionalStaticBranch := False
    brInfo.isConditionalBranch := fetch.io.output.payload.insn(31)
    brInfo.isUnconditionalDynamicBranch := False
    brInfo.backward := fetch.io.output.payload.insn(15)
    brInfo.target := fetch.io.output.payload.pc + (fetch.io.output.payload.insn(
      15 downto 0
    ) ## B(0, 2 bits)).asSInt.resize(32 bits).asUInt

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
        name = "test"
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
          for (i <- 0 until 1000000) {
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

        val brPatternCandidates = Seq(
          Seq(true),
          Seq(true),
          Seq(false),
          Seq(false),
          Seq(true, false),
          Seq(true, false, false),
          Seq(true, true, false),
          Seq(true, true, false, true, false, false)
        )

        val numWords = memSize / 4
        def encodeBr(offset: Int): Int = {
          assert(offset >= -32768 && offset < 32767)
          (1 << 31) | (offset & 0xffff)
        }

        val insnMem = (0 until numWords).map(i => {
          if (i == numWords - 1) {
            // Branch to head
            encodeBr(-(numWords - 1))
          } else {
            if (Random.nextInt(8) < 1) {
              // branch!
              val off = Random.nextInt(numWords - i - 1) + 1
              encodeBr(off)
            } else {
              0
            }
          }
        })
        val brSchedule = insnMem.zipWithIndex
          .map { case (insn, i) =>
            if ((insn & (1 << 31)) != 0) {
              if (i == numWords - 1) Seq(true)
              else
                brPatternCandidates(Random.nextInt(brPatternCandidates.length))
            } else null
          }
          .to[ArrayBuffer]

        // Write
        for ((word, i) <- insnMem.zipWithIndex) {
          val bytes = Seq(
            word & 0xff,
            (word >> 8) & 0xff,
            (word >> 16) & 0xff,
            (word >> 24) & 0xff
          ).map(x => x.toByte)
          dut.io.mem.simWriteBytes(dut, i * bytes.size, bytes)
        }
        println("Initialized instruction memory.")

        dut.io.fetchOut.ready #= true
        var exceptionPending = false

        var brMissCount = 0
        var brHitCount = 0
        var totalCount = 0

        // Randomly stall the pipeline
        fork {
          while (true) {
            dut.clockDomain.waitSampling()
            dut.io.fetchOut.ready #= Random.nextInt(10) < 8
          }
        }

        // Simulated backend
        fork {
          val pipelineDelay = 11
          def scheduleException(f: => Unit) {
            assert(!exceptionPending)
            exceptionPending = true
            fork {
              dut.clockDomain.waitSampling(pipelineDelay)
              f
              assert(exceptionPending)
              exceptionPending = false
            }
          }

          var expectingInsnIndex = 0

          while (true) {
            dut.clockDomain.waitSamplingWhere(
              dut.io.fetchOut.valid.toBoolean && dut.io.fetchOut.ready.toBoolean && !exceptionPending
            )
            totalCount += 1
            val insnIndex = dut.io.fetchOut.payload.pc.toBigInt.toInt / 4
            assert(
              insnIndex == expectingInsnIndex,
              "Unexpected instruction index"
            )
            val fp = dut.io.fetchOut_payload_bits.toBigInt
            if (dut.io.fetchOut.payload.cacheMiss.toBoolean) {
              println(
                "cache miss - scheduling INSN_CACHE_MISS - insnIndex " + insnIndex
              )
              scheduleException {
                dut.io.exc.exc.valid #= true
                dut.io.exc.exc.code #= MachineExceptionCode.INSN_CACHE_MISS
                dut.io.exc_fetchPacket_bits #= fp
                dut.clockDomain.waitSampling()
                dut.io.exc.exc.valid #= false
              }
            } else {
              assert(
                dut.io.fetchOut.payload.insn.toBigInt.toInt == insnMem(
                  insnIndex
                )
              )
              val br = brSchedule(insnIndex)
              if (br != null) {
                val decision = br.head
                brSchedule.update(insnIndex, br.drop(1) ++ br.take(1))
                val predicted =
                  dut.io.fetchOut.payload.predictedBranchValid.toBoolean

                val dst =
                  (insnIndex + insnMem(insnIndex).toInt.toShort.toInt) * 4
                if (predicted) {
                  assert(
                    dut.io.fetchOut.payload.predictedBranchTarget.toBigInt == dst
                  )
                }
                if (debug)
                  println(
                    "br at insnIndex " + insnIndex + " decision " + decision + " predicted " + predicted + " dst " + dst
                  )

                if (decision != predicted) {
                  if (debug)
                    println("prediction wrong - scheduling BRANCH_MISS")
                  scheduleException {
                    dut.io.exc.exc.valid #= true
                    dut.io.exc.exc.code #= MachineExceptionCode.BRANCH_MISS
                    dut.io.exc.exc.brDstAddr #= dst
                    dut.io.exc.exc.brIsConst #= true
                    dut.io.exc.exc.brTaken #= decision
                    dut.io.exc_fetchPacket_bits #= fp
                    dut.clockDomain.waitSampling()
                    dut.io.exc.exc.valid #= false
                  }
                  brMissCount += 1
                } else {
                  brHitCount += 1
                }

                if (decision) {
                  expectingInsnIndex = dst / 4
                } else {
                  expectingInsnIndex = insnIndex + 1
                }

              } else {
                if (debug) println("not branch at insnIndex " + insnIndex)
                expectingInsnIndex = insnIndex + 1
              }
            }
          }
        }

        dut.clockDomain.waitSampling(600000)
        println("total: " + totalCount)
        println("br miss: " + brMissCount)
        println("br hit: " + brHitCount)
        println("br hit rate: " + brHitCount.toDouble / (brHitCount + brMissCount).toDouble)
      }
  }
}
