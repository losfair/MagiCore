package miniooo.control

import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._

import org.scalatest.funsuite.AnyFunSuite
import miniooo.util.PolymorphicDataChain
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import miniooo.testutil.TestExt._
import scala.collection.mutable
import miniooo.lib.funit._
import scala.reflect._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.misc.SizeMapping
import miniooo.testutil.TestSyncResetSpinalConfig

class TestBackendPipeline extends AnyFunSuite {
  object TestTag {
    def static(tag: Int): TestTag = {
      val t = TestTag()
      assert(tag >= 0 && tag <= t.tag.maxValue)
      t.tag := tag
      t
    }
  }
  case class TestTag() extends Bundle {
    val tag = UInt(4 bits)
  }

  val ocmSize = 4096

  val mspec = MachineSpec(
    numArchitecturalRegs = 8,
    numPhysicalRegs = 64,
    addrWidth = 32 bits,
    dataWidth = 32 bits,
    maxNumSrcRegsPerInsn = 2,
    maxNumDstRegsPerInsn = 1,
    issueQueueSize = 16,
    functionUnitTagType = HardType(TestTag()),
    robSize = 32,
    commitWidth = 2
  )

  val msem = new MachineSemantics {
    lazy val functionUnits: Seq[FunctionUnit] = Seq(
      new Alu(TestTag.static(0), AluConfig(alu32 = false)),
      new Multiplier(TestTag.static(1), MultiplierConfig()),
      new Divider(TestTag.static(2)),
      new DummyEffect(TestTag.static(3)),
      new Lsu(TestTag.static(4), LsuConfig()),
      new EarlyExcPassthrough(TestTag.static(5))
    )
  }

  object GenericOpcode extends SpinalEnum(binarySequential) {
    val ADD, SUB, AND, OR, XOR, MOV, DIV_S, DIV_U, REM_S, REM_U, LD_B_U, LD_B_S,
        LD_H_U, LD_H_S, LD_W, ST_B, ST_H, ST_W, MFENCE, BLTU, BGE, BEQ, SERIALIZE =
      newElement()

    def translateToAlu(
        that: SpinalEnumCraft[GenericOpcode.type]
    ): (Bool, SpinalEnumCraft[AluOpcode.type]) = {
      val out = AluOpcode()
      val ok = True
      out.assignDontCare()
      switch(that) {
        is(ADD) { out := AluOpcode.ADD }
        is(SUB) { out := AluOpcode.SUB }
        is(AND) { out := AluOpcode.AND }
        is(OR) { out := AluOpcode.OR }
        is(XOR) { out := AluOpcode.XOR }
        is(MOV) { out := AluOpcode.MOV }
        is(BLTU, BGE, BEQ) { out := AluOpcode.BRANCH }
        default { ok := False }
      }
      (ok, out)
    }

    def getBrCond(
        that: SpinalEnumCraft[GenericOpcode.type]
    ): SpinalEnumCraft[AluBranchCondition.type] = {
      val out = AluBranchCondition()
      out.assignDontCare()
      switch(that) {
        is(BLTU) { out := AluBranchCondition.LTU }
        is(BGE) { out := AluBranchCondition.GE }
        is(BEQ) { out := AluBranchCondition.EQ }
      }
      out
    }
  }

  case class MockPayload() extends Bundle with PolymorphicDataChain {
    val decode = DecodeInfo(null)
    val const = UInt(32 bits)
    val useConst = Bool()
    val opc = GenericOpcode()
    val brCtx = AluBranchContext(globalHistoryWidth = 8 bits)
    val setPredicateInsteadOfBranch = Bool()
    def parentObjects: Seq[Data] = Seq(decode, brCtx)

    override def decodeAs[T <: AnyRef](ctag: ClassTag[T]): Option[T] = {
      if (ctag == classTag[AluOperation]) {
        val op = AluOperation()
        op.alu32 := False
        op.const := const.asBits
        op.opcode := GenericOpcode.translateToAlu(opc)._2
        op.predicated := False
        op.useConst := useConst
        op.brCond := GenericOpcode.getBrCond(opc)
        op.setPredicateInsteadOfBranch := setPredicateInsteadOfBranch
        Some(op.asInstanceOf[T])
      } else if (ctag == classTag[DividerOperation]) {
        val op = DividerOperation()
        op.signed := opc === GenericOpcode.DIV_S || opc === GenericOpcode.REM_S
        op.useRemainder := opc === GenericOpcode.REM_S || opc === GenericOpcode.REM_U
        Some(op.asInstanceOf[T])
      } else if (ctag == classTag[DummyEffectOperation]) {
        val op = DummyEffectOperation()
        op.value := const
        Some(op.asInstanceOf[T])
      } else if (ctag == classTag[LsuOperation]) {
        val op = LsuOperation()
        op.isFence := opc === GenericOpcode.MFENCE
        op.isStore := opc === GenericOpcode.ST_B || opc === GenericOpcode.ST_H || opc === GenericOpcode.ST_W
        op.size := opc.mux(
          GenericOpcode.ST_B -> LsuOperationSize.BYTE.craft(),
          GenericOpcode.LD_B_U -> LsuOperationSize.BYTE.craft(),
          GenericOpcode.LD_B_S -> LsuOperationSize.BYTE.craft(),
          GenericOpcode.ST_H -> LsuOperationSize.HALF.craft(),
          GenericOpcode.LD_H_U -> LsuOperationSize.HALF.craft(),
          GenericOpcode.LD_H_S -> LsuOperationSize.HALF.craft(),
          default -> LsuOperationSize.WORD.craft()
        )
        op.signExt := opc === GenericOpcode.LD_B_S || opc === GenericOpcode.LD_H_S
        op.offset := const.asSInt
        Some(op.asInstanceOf[T])
      } else if (ctag == classTag[EarlyException]) {
        val op = EarlyException()
        op.code := opc.mux(
          GenericOpcode.SERIALIZE -> EarlyExceptionCode.SERIALIZE.craft(),
          default -> EarlyExceptionCode.DECODE_ERROR.craft()
        )
        Some(op.asInstanceOf[T])
      } else {
        None
      }
    }
  }

  object MockPayload {
    def create(
        out: MockPayload,
        t: Int,
        rs1: Option[Int] = None,
        rs2: Option[Int] = None,
        rd: Option[Int] = None,
        const: Option[BigInt] = None,
        setPredicateInsteadOfBranch: Boolean = false,
        opc: SpinalEnumElement[GenericOpcode.type],
        predictedBranch: Option[BigInt] = None
    ) {
      out.decode.archSrcRegs(0).valid #= rs1.isDefined
      out.decode.archSrcRegs(0).index #= rs1.getOrElse(0)
      out.decode.archSrcRegs(1).valid #= rs2.isDefined
      out.decode.archSrcRegs(1).index #= rs2.getOrElse(0)
      out.decode.archDstRegs(0).valid #= rd.isDefined
      out.decode.archDstRegs(0).index #= rd.getOrElse(0)
      out.decode.functionUnitTag.asInstanceOf[TestTag].tag #= t
      out.const #= const.getOrElse(BigInt(0))
      out.useConst #= const.isDefined
      out.setPredicateInsteadOfBranch #= setPredicateInsteadOfBranch
      out.brCtx.pc #= 0
      out.brCtx.predictedBranchValid #= predictedBranch.isDefined
      if (predictedBranch.isDefined) {
        out.brCtx.predictedBranchTarget #= predictedBranch.get
      }
      out.opc #= opc
    }
  }

  class TestBackendPipelineTop(debug: Boolean = false) extends Component {
    if (debug) Machine.provide(MachineDebugMarker)
    Machine.provide(mspec)
    Machine.provide(msem)
    val pipeline = BackendPipeline(MockPayload())
    val lsu =
      pipeline.lookupFunctionUnitInstancesByType(classOf[LsuInstance]).head
    val ocram = new Axi4SharedOnChipRam(
      dataWidth = lsu.io_axiMaster.config.dataWidth,
      byteCount = ocmSize,
      idWidth = lsu.io_axiMaster.config.idWidth + 1
    )

    val io = new Bundle {
      val input = slave(Stream(MockPayload()))

      val regReadAddr = in(Machine.get[MachineSpec].archRegIndexType)
      val regReadData = out(Machine.get[MachineSpec].dataType)
      val writebackMonitor = out(
        Vec(
          pipeline.io.writebackMonitor.dataType(),
          pipeline.io.writebackMonitor.size
        )
      )
      val cycles = out(UInt(64 bits))
      val effectOutput = out(Flow(UInt(32 bits)))
      val memBus = slave(Axi4Shared(lsu.io_axiMaster.config))
      val backendReset = out(Bool())
    }
    io.input >> pipeline.io.input
    val prfIf = Machine.get[PrfInterface]
    io.regReadData := prfIf.readAsync(pipeline.rename.cmt(io.regReadAddr)).data
    pipeline.io.writebackMonitor
      .zip(io.writebackMonitor)
      .foreach(x => x._1 >-> x._2)

    val cycles = Reg(UInt(64 bits)) init (0)
    cycles := cycles + 1

    io.effectOutput := pipeline
      .lookupFunctionUnitInstancesByType(classOf[DummyEffectInstance])(0)
      .effectOutput

    io.cycles := cycles

    Axi4CrossbarFactory()
      .addSlave(ocram.io.axi, SizeMapping(0, ocmSize))
      .addConnections(
        lsu.io_axiMaster -> Seq(ocram.io.axi),
        io.memBus -> Seq(ocram.io.axi)
      )
      .build()

    io.backendReset := Machine.get[MachineException].valid
  }

  test("TestBackendPipeline") {
    val debug = false
    SimConfig
      .withConfig(TestSyncResetSpinalConfig)
      .withWave
      .doSim(
        rtl = Machine.build { new TestBackendPipelineTop(debug = debug) },
        name = "test"
      ) { dut =>
        dut.io.input.valid #= false
        dut.io.memBus.arw.ready #= false
        dut.io.memBus.w.ready #= false
        dut.io.memBus.b.valid #= false
        dut.io.memBus.r.valid #= false

        dut.clockDomain.forkStimulus(100)
        waitUntil(dut.clockDomain.isResetAsserted)
        waitUntil(dut.clockDomain.isResetDeasserted)

        if (debug) fork {
          var cycles = 0
          while (true) {
            dut.clockDomain.waitFallingEdge()
            println("+++ cycle " + cycles + " +++")
            cycles += 1
            // if(cycles == 3900) throw new Exception("Stop!")
          }
        }

        var memMirror = (0 until ocmSize / (mspec.dataWidth.value / 8))
          .map(_ => BigInt(Random.nextInt(1000000000)))
          .to[ArrayBuffer]

        // Init memory
        for (i <- 0 until ocmSize / (mspec.dataWidth.value / 8)) {
          val wordOffset = i * (mspec.dataWidth.value / 8)
          dut.io.input.simWrite(
            dut,
            p => {
              MockPayload.create(
                p,
                t = 0,
                rs1 = None,
                rs2 = None,
                const = Some(wordOffset),
                rd = Some(0),
                opc = GenericOpcode.MOV
              )
            }
          )
          dut.io.input.simWrite(
            dut,
            p => {
              MockPayload.create(
                p,
                t = 0,
                rs1 = None,
                rs2 = None,
                const = Some(memMirror(i)),
                rd = Some(1),
                opc = GenericOpcode.MOV
              )
            }
          )
          dut.io.input.simWrite(
            dut,
            p => {
              MockPayload.create(
                p,
                t = 4,
                rs1 = Some(0),
                rs2 = Some(1),
                const = Some(0),
                rd = None,
                opc = GenericOpcode.ST_W
              )
            }
          )
        }

        var mirror =
          (0 until mspec.numArchitecturalRegs)
            .map(_ => BigInt(Random.nextInt(100000)))
            .to[ArrayBuffer]

        var expectingException = false
        var memMirror_excSnapshot = memMirror.toSeq.to[ArrayBuffer]
        var mirror_excSnapshot = mirror.toSeq.to[ArrayBuffer]

        // Init registers
        for (i <- 0 until mspec.numArchitecturalRegs) {
          val value = mirror(i)
          dut.io.input.simWrite(
            dut,
            p => {
              MockPayload.create(
                p,
                t = 0,
                rs1 = None,
                rs2 = None,
                const = Some(value),
                rd = Some(i),
                opc = GenericOpcode.MOV
              )
            }
          )
        }

        dut.clockDomain.waitSampling(100) // wait for preparation
        println("preparation done")

        val caseCount: mutable.Map[String, Int] = mutable.Map()
        caseCount.update("LD_CONST", 0)
        caseCount.update("ADD", 0)
        caseCount.update("MUL", 0)
        caseCount.update("DIV_U", 0)
        caseCount.update("DUMMY_EFFECT", 0)
        caseCount.update("LD_W", 0)
        caseCount.update("LD_B_U", 0)
        caseCount.update("LD_B_S", 0)
        caseCount.update("ST_W", 0)
        caseCount.update("ST_B", 0)
        caseCount.update("BR_HIT", 0)
        caseCount.update("BR_MISS", 0)
        caseCount.update("MFENCE", 0)
        caseCount.update("SERIALIZE", 0)

        val testSize = 200000
        var writebackCount = 0
        var exceptionCount = 0

        var insnCount = 0
        var insnCount_excSnapshot = 0
        val expectedWritebackLog = ArrayBuffer[(Int, BigInt)]()
        val actualWritebackLog = ArrayBuffer[(Int, BigInt)]()

        fork {
          while (true) {
            dut.clockDomain.waitSampling()
            for (ch <- dut.io.writebackMonitor) {
              if (ch.valid.toBoolean) {
                val decode = ch.payload.lookup[DecodeInfo]
                if (
                  decode
                    .archDstRegs(0)
                    .valid
                    .toBoolean && !ch.payload.exception.valid.toBoolean
                ) {
                  val logIndex = actualWritebackLog.size
                  val (index, value) = (
                    decode.archDstRegs(0).index.toInt,
                    ch.payload.regWriteValue(0).toBigInt
                  )
                  actualWritebackLog += ((index, value))

                  if (logIndex >= expectedWritebackLog.size)
                    throw new Exception(
                      "actual writeback log runs ahead of expected"
                    )
                  val expected = expectedWritebackLog(logIndex)
                  assert((index, value) == expected, "log validation failed")
                  if (debug) println("validated writeback: " + (index, value))
                }
                writebackCount += 1
              }
            }
          }
        }

        // Watch effect
        var nextDummyEffectValue = 0
        fork {
          while (true) {
            dut.clockDomain.waitSampling()
            if (dut.io.effectOutput.valid.toBoolean) {
              val value = dut.io.effectOutput.payload.toBigInt
              assert(value == nextDummyEffectValue)
              nextDummyEffectValue += 1
            }
          }
        }

        val wordMask = ocmSize - ((1 << log2Up(mspec.dataWidth.value / 8)))

        val startCycles = dut.io.cycles.toBigInt
        var delayCount = 0
        var dummyEffectSeq = 0
        var dummyEffectSeq_excSnapshot = 0
        val printInsn = debug
        var exceptionPending = false

        val writeIt = (f: MockPayload => Unit) => {
          dut.io.input.simWrite(
            dut,
            f,
            giveUp = exceptionPending || dut.io.backendReset.toBoolean
          )
        }

        val checkPendingException = () => {
          if (exceptionPending) {
            assert(expectingException, "got unexpected exception")
            expectingException = false
            if (debug)
              println("handling exception at cyc " + dut.io.cycles.toBigInt)
            memMirror = memMirror_excSnapshot
            mirror = mirror_excSnapshot
            insnCount = insnCount_excSnapshot
            exceptionPending = false
            dummyEffectSeq = dummyEffectSeq_excSnapshot
            if (printInsn) println("---- discarded range end ---")
          }
        }

        fork {
          while (true) {
            dut.clockDomain.waitSampling()
            if (dut.io.backendReset.toBoolean) {
              if (exceptionPending) {
                throw new Exception("conflicting exception")
              }
              dut.clockDomain.waitFallingEdge()
              exceptionPending = true
              dut.clockDomain.waitSampling()
            }
          }
        }

        def scheduleException() {
          if (!expectingException) {
            exceptionCount += 1
            expectingException = true
            memMirror_excSnapshot = memMirror.toSeq.to[ArrayBuffer]
            mirror_excSnapshot = mirror.toSeq.to[ArrayBuffer]
            insnCount_excSnapshot = insnCount
            dummyEffectSeq_excSnapshot = dummyEffectSeq
            if (printInsn) println("---- discarded range start ---")
          }
        }

        for (i <- 0 until testSize) {
          checkPendingException()
          insnCount += 1
          val thisDelay = if (Random.nextInt(50) < 2) Random.nextInt(5) else 0
          if (thisDelay != 0) {
            delayCount += thisDelay
            dut.clockDomain.waitSampling(thisDelay)
          }
          val op = Random.nextInt(126)
          op match {
            case x if 0 until 10 contains x =>
              // LD_CONST
              caseCount.update("LD_CONST", caseCount("LD_CONST") + 1)
              val dst = Random.nextInt(mspec.numArchitecturalRegs)
              val value = Random.nextInt(100000)
              if (printInsn) println("ld_const " + value + " -> r" + dst)
              mirror.update(dst, value)
              if (!expectingException)
                expectedWritebackLog += ((dst, mirror(dst)))
              writeIt(p => {
                MockPayload.create(
                  p,
                  t = 0,
                  rs1 = None,
                  rs2 = None,
                  const = Some(value),
                  rd = Some(dst),
                  opc = GenericOpcode.MOV
                )
              })
            case x if 10 until 70 contains x =>
              // ADD
              caseCount.update("ADD", caseCount("ADD") + 1)
              val left = Random.nextInt(mspec.numArchitecturalRegs)
              val right = Random.nextInt(mspec.numArchitecturalRegs)
              val dst = Random.nextInt(mspec.numArchitecturalRegs)
              if (printInsn)
                println("add r" + left + " r" + right + " -> r" + dst)
              mirror.update(dst, (mirror(left) + mirror(right)) & 0xffffffffL)
              if (!expectingException)
                expectedWritebackLog += ((dst, mirror(dst)))
              writeIt(p => {
                MockPayload.create(
                  p,
                  t = 0,
                  rs1 = Some(left),
                  rs2 = Some(right),
                  rd = Some(dst),
                  opc = GenericOpcode.ADD
                )
              })
            case x if 70 until 90 contains x =>
              // MUL
              caseCount.update("MUL", caseCount("MUL") + 1)
              val left = Random.nextInt(mspec.numArchitecturalRegs)
              val right = Random.nextInt(mspec.numArchitecturalRegs)
              val dst = Random.nextInt(mspec.numArchitecturalRegs)
              if (printInsn)
                println("mul r" + left + " r" + right + " -> r" + dst)
              mirror.update(dst, (mirror(left) * mirror(right)) & 0xffffffffL)
              if (!expectingException)
                expectedWritebackLog += ((dst, mirror(dst)))
              writeIt(p => {
                MockPayload.create(
                  p,
                  t = 1,
                  rs1 = Some(left),
                  rs2 = Some(right),
                  rd = Some(dst),
                  opc = GenericOpcode.ADD
                )
              })
            case x if 90 until 92 contains x => {
              // DIV_U
              caseCount.update("DIV_U", caseCount("DIV_U") + 1)
              val left = Random.nextInt(mspec.numArchitecturalRegs)
              val right = Random.nextInt(mspec.numArchitecturalRegs)
              val dst = Random.nextInt(mspec.numArchitecturalRegs)
              if (printInsn)
                println("div_u r" + left + " r" + right + " -> r" + dst)
              if (mirror(right) == 0) {
                mirror.update(dst, 0xffffffffL)
              } else {
                mirror.update(dst, mirror(left) / mirror(right))
              }
              if (!expectingException)
                expectedWritebackLog += ((dst, mirror(dst)))
              writeIt(p => {
                MockPayload.create(
                  p,
                  t = 2,
                  rs1 = Some(left),
                  rs2 = Some(right),
                  rd = Some(dst),
                  opc = GenericOpcode.DIV_U
                )
              })
            }
            case x if 92 until 100 contains x => {
              // DUMMY_EFFECT
              caseCount.update("DUMMY_EFFECT", caseCount("DUMMY_EFFECT") + 1)
              val rd =
                if (Random.nextBoolean())
                  Some(Random.nextInt(mspec.numArchitecturalRegs))
                else None
              val rs1 = Random.nextInt(mspec.numArchitecturalRegs)
              val rs2 =
                if (Random.nextBoolean())
                  Some(Random.nextInt(mspec.numArchitecturalRegs))
                else None
              if (rd.isDefined) {
                mirror(rd.get) = (mirror(rs1) + 42) & 0xffffffffL
                if (!expectingException)
                  expectedWritebackLog += ((rd.get, mirror(rd.get)))
              }
              if (printInsn)
                println(
                  "dummy_effect r" + rs1 + " r" + rs2 + " -> r" + rd + " seq=" + dummyEffectSeq
                )
              writeIt(p => {
                MockPayload.create(
                  p,
                  t = 3,
                  rs1 = Some(rs1),
                  rs2 = rs2,
                  rd = rd,
                  const = Some(dummyEffectSeq),
                  opc = GenericOpcode.ADD
                )
              })
              dummyEffectSeq += 1
            }
            case x if 100 until 105 contains x => {
              // LD_W
              caseCount.update("LD_W", caseCount("LD_W") + 1)
              val rd = Random.nextInt(mspec.numArchitecturalRegs)
              val rs1 = Random.nextInt(mspec.numArchitecturalRegs)

              val mask = (wordMask >> 2) << 2

              val addr = mirror(rs1) & mask
              val data = memMirror((addr / (mspec.dataWidth.value / 8)).toInt)
              mirror.update(rd, data)
              if (!expectingException) {
                expectedWritebackLog += ((rd, addr))
                expectedWritebackLog += ((rd, mirror(rd)))
              }
              if (printInsn)
                println(
                  "ld_w: r" + rs1 + "(" + addr
                    .hexString() + ") -> r" + rd + ", data: " + data.hexString()
                )

              // mask
              writeIt(p => {
                MockPayload.create(
                  p,
                  t = 0,
                  rs1 = Some(rs1),
                  rs2 = None,
                  const = Some(mask),
                  rd = Some(rd),
                  opc = GenericOpcode.AND
                )
              })

              writeIt(p => {
                MockPayload.create(
                  p,
                  t = 4,
                  rs1 = Some(rd),
                  rs2 = None,
                  const = Some(0),
                  rd = Some(rd),
                  opc = GenericOpcode.LD_W
                )
              })
              insnCount += 1
            }
            case x if 105 until 108 contains x => {
              // LD_B_U
              caseCount.update("LD_B_U", caseCount("LD_B_U") + 1)
              val rd = Random.nextInt(mspec.numArchitecturalRegs)
              val rs1 = Random.nextInt(mspec.numArchitecturalRegs)

              val mask = wordMask

              val addr = mirror(rs1) & mask
              val bitShift = (addr & ((mspec.dataWidth.value / 8) - 1)) * 8
              val data = (memMirror(
                (addr / (mspec.dataWidth.value / 8)).toInt
              ) >> bitShift.toInt) & 0xff
              mirror.update(rd, data)
              if (!expectingException) {
                expectedWritebackLog += ((rd, addr))
                expectedWritebackLog += ((rd, mirror(rd)))
              }
              if (printInsn)
                println(
                  "ld_b_u: r" + rs1 + "(" + addr
                    .hexString() + ") -> r" + rd + ", data: " + data.hexString()
                )

              // mask
              writeIt(p => {
                MockPayload.create(
                  p,
                  t = 0,
                  rs1 = Some(rs1),
                  rs2 = None,
                  const = Some(mask),
                  rd = Some(rd),
                  opc = GenericOpcode.AND
                )
              })

              writeIt(p => {
                MockPayload.create(
                  p,
                  t = 4,
                  rs1 = Some(rd),
                  rs2 = None,
                  const = Some(0),
                  rd = Some(rd),
                  opc = GenericOpcode.LD_B_U
                )
              })
              insnCount += 1
            }
            case x if 108 until 110 contains x => {
              // LD_B_S
              caseCount.update("LD_B_S", caseCount("LD_B_S") + 1)
              val rd = Random.nextInt(mspec.numArchitecturalRegs)
              val rs1 = Random.nextInt(mspec.numArchitecturalRegs)

              val mask = wordMask

              val addr = mirror(rs1) & mask
              val bitShift = (addr & ((mspec.dataWidth.value / 8) - 1)) * 8
              val data_ = (memMirror(
                (addr / (mspec.dataWidth.value / 8)).toInt
              ) >> bitShift.toInt) & 0xff
              val data = if ((data_ >> 7) == 0) data_ else (data_ | 0xffffff00L)
              mirror.update(rd, data)
              if (!expectingException) {
                expectedWritebackLog += ((rd, addr))
                expectedWritebackLog += ((rd, mirror(rd)))
              }
              if (printInsn)
                println(
                  "ld_b_s: r" + rs1 + "(" + addr
                    .hexString() + ") -> r" + rd + ", data: " + data.hexString()
                )

              // mask
              writeIt(p => {
                MockPayload.create(
                  p,
                  t = 0,
                  rs1 = Some(rs1),
                  rs2 = None,
                  const = Some(mask),
                  rd = Some(rd),
                  opc = GenericOpcode.AND
                )
              })

              writeIt(p => {
                MockPayload.create(
                  p,
                  t = 4,
                  rs1 = Some(rd),
                  rs2 = None,
                  const = Some(0),
                  rd = Some(rd),
                  opc = GenericOpcode.LD_B_S
                )
              })
              insnCount += 1
            }
            case x if 110 until 115 contains x => {
              // ST_W
              caseCount.update("ST_W", caseCount("ST_W") + 1)
              val rs1 = Random.nextInt(mspec.numArchitecturalRegs)
              val mask = (wordMask >> 2) << 2
              val addr = mirror(rs1) & mask
              mirror.update(rs1, addr)
              if (!expectingException) {
                expectedWritebackLog += ((rs1, addr))
              }

              // Read rs2 after updating rs1, in case rs1 == rs2.
              val rs2 = Random.nextInt(mspec.numArchitecturalRegs)
              memMirror.update(
                (addr / (mspec.dataWidth.value / 8)).toInt,
                mirror(rs2)
              )
              if (printInsn)
                println(
                  "st_w: r" + rs1 + "(" + addr
                    .hexString() + ") <- mem[r" + rs2 + "], data: " + mirror(
                    rs2
                  )
                    .hexString()
                )

              // mask
              writeIt(p => {
                MockPayload.create(
                  p,
                  t = 0,
                  rs1 = Some(rs1),
                  rs2 = None,
                  const = Some(mask),
                  rd = Some(rs1),
                  opc = GenericOpcode.AND
                )
              })

              writeIt(p => {
                MockPayload.create(
                  p,
                  t = 4,
                  rs1 = Some(rs1),
                  rs2 = Some(rs2),
                  const = Some(0),
                  rd = None,
                  opc = GenericOpcode.ST_W
                )
              })

              insnCount += 1
            }
            case x if 115 until 120 contains x => {
              // ST_B
              caseCount.update("ST_B", caseCount("ST_B") + 1)
              val rs1 = Random.nextInt(mspec.numArchitecturalRegs)
              val mask = wordMask
              val addr = mirror(rs1) & mask
              mirror.update(rs1, addr)
              if (!expectingException) {
                expectedWritebackLog += ((rs1, addr))
              }

              // Read rs2 after updating rs1, in case rs1 == rs2.
              val rs2 = Random.nextInt(mspec.numArchitecturalRegs)
              val wordIndex = (addr / (mspec.dataWidth.value / 8)).toInt
              val byteIndex = addr & ((mspec.dataWidth.value / 8) - 1)
              val newValue = BigInt(
                memMirror(wordIndex).toByteArray.toSeq.reverse
                  .padTo(4, 0.toByte)
                  .updated(byteIndex.toInt, mirror(rs2).toByte)
                  .reverse
                  .toArray
              )
              memMirror.update(
                wordIndex,
                newValue
              )
              if (printInsn)
                println(
                  "st_b: r" + rs1 + "(" + addr
                    .hexString() + ") <- r" + rs2 + ", data: " + mirror(
                    rs2
                  ).toByte
                    .hexString()
                )

              // mask
              writeIt(p => {
                MockPayload.create(
                  p,
                  t = 0,
                  rs1 = Some(rs1),
                  rs2 = None,
                  const = Some(mask),
                  rd = Some(rs1),
                  opc = GenericOpcode.AND
                )
              })

              writeIt(p => {
                MockPayload.create(
                  p,
                  t = 4,
                  rs1 = Some(rs1),
                  rs2 = Some(rs2),
                  const = Some(0),
                  rd = None,
                  opc = GenericOpcode.ST_B
                )
              })

              insnCount += 1
            }
            case x if 120 until 121 contains x => {
              // BRANCH
              val prediction = Random.nextBoolean()
              val rs1 = Random.nextInt(mspec.numArchitecturalRegs)
              val rs2 = Random.nextInt(mspec.numArchitecturalRegs)
              val shouldBranch = mirror(rs1) < mirror(rs2)
              val predictionHit = prediction == shouldBranch
              val predicted = if (prediction) Some(BigInt(12)) else None

              if (prediction != shouldBranch) {
                if (printInsn)
                  println(
                    "br_miss r" + rs1 + "(" + mirror(rs1)
                      .hexString() + ") r" + rs2 + "(" + mirror(rs2)
                      .hexString() + ")"
                  )
                caseCount.update("BR_MISS", caseCount("BR_MISS") + 1)
                scheduleException()
              } else {
                if (printInsn)
                  println(
                    "br_hit r" + rs1 + "(" + mirror(rs1)
                      .hexString() + ") r" + rs2 + "(" + mirror(rs2)
                      .hexString() + ")"
                  )
                caseCount.update("BR_HIT", caseCount("BR_HIT") + 1)
              }

              writeIt(p => {
                MockPayload.create(
                  p,
                  t = 0,
                  rs1 = Some(rs1),
                  rs2 = Some(rs2),
                  const = Some(12),
                  rd = None,
                  opc = GenericOpcode.BLTU,
                  predictedBranch = predicted
                )
              })
            }
            case x if 121 until 125 contains x => {
              // MFENCE

              if (printInsn)
                println("mfence")

              caseCount.update("MFENCE", caseCount("MFENCE") + 1)
              writeIt(p => {
                MockPayload.create(
                  p,
                  t = 4,
                  rs1 = None,
                  rs2 = None,
                  const = None,
                  rd = None,
                  opc = GenericOpcode.MFENCE
                )
              })
            }
            case x if 125 until 126 contains x => {
              // SERIALIZE

              if (printInsn)
                println("serialize")

              caseCount.update("SERIALIZE", caseCount("SERIALIZE") + 1)
              writeIt(p => {
                MockPayload.create(
                  p,
                  t = 5,
                  rs1 = None,
                  rs2 = None,
                  const = None,
                  rd = None,
                  opc = GenericOpcode.SERIALIZE
                )
              })
              scheduleException()
            }
          }
        }

        while (writebackCount < insnCount) {
          checkPendingException()
          dut.clockDomain.waitSampling()
        }

        val endCycles = dut.io.cycles.toBigInt
        println(
          "Finished " + insnCount + " instructions in " + (endCycles - startCycles) + " cycles. IPC=" + (insnCount.toDouble / (endCycles - startCycles).toDouble)
        )
        println(
          "Total inserted delay is " + delayCount + " so minimum possible cycle count is " + (insnCount + delayCount) + "."
        )

        dut.clockDomain.waitSampling(100)
        assert(writebackCount == insnCount)

        for ((k, v) <- caseCount) {
          println(k + ": " + v)
        }

        println("Total number of exceptions triggered: " + exceptionCount)

        for (i <- 0 until mspec.numArchitecturalRegs) {
          dut.io.regReadAddr #= i
          dut.clockDomain.waitSampling()
          val data = dut.io.regReadData.toBigInt
          assert(data == mirror(i), "value validation failed for reg " + i)
          println("reg " + i + ": " + data)
        }

        assert(dummyEffectSeq == nextDummyEffectValue)
        println("validation ok")
      }
  }
}
