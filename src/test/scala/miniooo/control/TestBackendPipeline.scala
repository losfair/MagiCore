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
      new Lsu(TestTag.static(4), LsuConfig())
    )
  }

  object GenericOpcode extends SpinalEnum(binarySequential) {
    val ADD, SUB, AND, OR, XOR, MOV, DIV_S, DIV_U, REM_S, REM_U, LD, ST =
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
        default { ok := False }
      }
      (ok, out)
    }
  }

  case class MockPayload() extends Bundle with PolymorphicDataChain {
    val decode = DecodeInfo(null)
    val const = UInt(32 bits)
    val useConst = Bool()
    val opc = GenericOpcode()
    def parentObjects: Seq[Data] = Seq(decode)

    override def decodeAs[T <: AnyRef](ctag: ClassTag[T]): Option[T] = {
      if (ctag == classTag[AluOperation]) {
        val op = AluOperation()
        op.alu32 := False
        op.const := const.asBits
        op.opcode := GenericOpcode.translateToAlu(opc)._2
        op.useConst := useConst
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
        op.isStore := opc === GenericOpcode.ST
        op.offset := const.asSInt
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
        opc: SpinalEnumElement[GenericOpcode.type]
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
      out.opc #= opc
    }
  }

  class TestBackendPipelineTop extends Component {
    // Machine.provide(MachineDebugMarker)
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
    }
    io.input >> pipeline.io.input
    val prfIf = Machine.get[PrfInterface]
    io.regReadData := prfIf.readAsync(pipeline.rename.cmt(io.regReadAddr)).data
    pipeline.io.writebackMonitor
      .zip(io.writebackMonitor)
      .foreach(x => x._1 >> x._2)

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
  }

  test("TestBackendPipeline") {
    SimConfig.withWave.doSim(
      rtl = Machine.build { new TestBackendPipelineTop() },
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

      val memMirror = (0 until ocmSize / (mspec.dataWidth.value / 8))
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
              opc = GenericOpcode.ST
            )
          }
        )
      }

      val mirror =
        (0 until mspec.numArchitecturalRegs)
          .map(_ => BigInt(Random.nextInt(100000)))
          .to[ArrayBuffer]

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
      caseCount.update("LD", 0)
      caseCount.update("ST", 0)

      val testSize = 50000
      var writebackCount = 0
      var expectedExtraWritebackCount = 0

      fork {
        while (true) {
          dut.clockDomain.waitSampling()
          for (ch <- dut.io.writebackMonitor) {
            if (ch.valid.toBoolean) {
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
      val printInsn = false

      for (i <- 0 until testSize) {
        val thisDelay = if (Random.nextInt(50) < 2) Random.nextInt(5) else 0
        if (thisDelay != 0) {
          delayCount += thisDelay
          dut.clockDomain.waitSampling(thisDelay)
        }
        val op = Random.nextInt(120)
        op match {
          case x if 0 until 10 contains x =>
            // LD_CONST
            caseCount.update("LD_CONST", caseCount("LD_CONST") + 1)
            val dst = Random.nextInt(mspec.numArchitecturalRegs)
            val value = Random.nextInt(100000)
            if(printInsn) println("ld_const " + value + " -> r" + dst)
            mirror.update(dst, value)
            dut.io.input.simWrite(
              dut,
              p => {
                MockPayload.create(
                  p,
                  t = 0,
                  rs1 = None,
                  rs2 = None,
                  const = Some(value),
                  rd = Some(dst),
                  opc = GenericOpcode.MOV
                )
              }
            )
          case x if 10 until 70 contains x =>
            // ADD
            caseCount.update("ADD", caseCount("ADD") + 1)
            val left = Random.nextInt(mspec.numArchitecturalRegs)
            val right = Random.nextInt(mspec.numArchitecturalRegs)
            val dst = Random.nextInt(mspec.numArchitecturalRegs)
            if(printInsn) println("add r" + left + " r" + right + " -> r" + dst)
            mirror.update(dst, (mirror(left) + mirror(right)) & 0xffffffffL)
            dut.io.input.simWrite(
              dut,
              p => {
                MockPayload.create(
                  p,
                  t = 0,
                  rs1 = Some(left),
                  rs2 = Some(right),
                  rd = Some(dst),
                  opc = GenericOpcode.ADD
                )
              }
            )
          case x if 70 until 90 contains x =>
            // MUL
            caseCount.update("MUL", caseCount("MUL") + 1)
            val left = Random.nextInt(mspec.numArchitecturalRegs)
            val right = Random.nextInt(mspec.numArchitecturalRegs)
            val dst = Random.nextInt(mspec.numArchitecturalRegs)
            if(printInsn) println("mul r" + left + " r" + right + " -> r" + dst)
            mirror.update(dst, (mirror(left) * mirror(right)) & 0xffffffffL)
            dut.io.input.simWrite(
              dut,
              p => {
                MockPayload.create(
                  p,
                  t = 1,
                  rs1 = Some(left),
                  rs2 = Some(right),
                  rd = Some(dst),
                  opc = GenericOpcode.ADD
                )
              }
            )
          case x if 90 until 92 contains x => {
            // DIV_U
            caseCount.update("DIV_U", caseCount("DIV_U") + 1)
            val left = Random.nextInt(mspec.numArchitecturalRegs)
            val right = Random.nextInt(mspec.numArchitecturalRegs)
            val dst = Random.nextInt(mspec.numArchitecturalRegs)
            if(printInsn) println("div_u r" + left + " r" + right + " -> r" + dst)
            if (mirror(right) == 0) {
              mirror.update(dst, 0xffffffffL)
            } else {
              mirror.update(dst, mirror(left) / mirror(right))
            }
            dut.io.input.simWrite(
              dut,
              p => {
                MockPayload.create(
                  p,
                  t = 2,
                  rs1 = Some(left),
                  rs2 = Some(right),
                  rd = Some(dst),
                  opc = GenericOpcode.DIV_U
                )
              }
            )
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
              mirror(rd.get) = mirror(rs1) + 42
            }
            if(printInsn) println("dummy_effect r" + rs1 + " r" + rs2 + " -> r" + rd)
            dut.io.input.simWrite(
              dut,
              p => {
                MockPayload.create(
                  p,
                  t = 3,
                  rs1 = Some(rs1),
                  rs2 = rs2,
                  rd = rd,
                  const = Some(dummyEffectSeq),
                  opc = GenericOpcode.ADD
                )
              }
            )
            dummyEffectSeq += 1
          }
          case x if 100 until 110 contains x => {
            // LD
            caseCount.update("LD", caseCount("LD") + 1)
            val rd = Random.nextInt(mspec.numArchitecturalRegs)
            val rs1 = Random.nextInt(mspec.numArchitecturalRegs)

            val addr = mirror(rs1) & wordMask
            val data = memMirror((addr / (mspec.dataWidth.value / 8)).toInt)
            mirror.update(rd, data)
            if(printInsn) println(
              "request load: r" + rs1 + "(" + addr
                .hexString() + ") -> r" + rd + ", data: " + data.hexString()
            )

            // mask
            dut.io.input.simWrite(
              dut,
              p => {
                MockPayload.create(
                  p,
                  t = 0,
                  rs1 = Some(rs1),
                  rs2 = None,
                  const = Some(wordMask),
                  rd = Some(rd),
                  opc = GenericOpcode.AND
                )
              }
            )

            dut.io.input.simWrite(
              dut,
              p => {
                MockPayload.create(
                  p,
                  t = 4,
                  rs1 = Some(rd),
                  rs2 = None,
                  const = Some(0),
                  rd = Some(rd),
                  opc = GenericOpcode.LD
                )
              }
            )

            expectedExtraWritebackCount += 1
          }
          case x if 110 until 120 contains x => {
            // ST
            caseCount.update("ST", caseCount("ST") + 1)
            val rs1 = Random.nextInt(mspec.numArchitecturalRegs)
            val addr = mirror(rs1) & wordMask
            mirror.update(rs1, addr)

            // Read rs2 after updating rs1, in case rs1 == rs2.
            val rs2 = Random.nextInt(mspec.numArchitecturalRegs)
            memMirror.update(
              (addr / (mspec.dataWidth.value / 8)).toInt,
              mirror(rs2)
            )
            if(printInsn) println(
              "request store: r" + rs1 + "(" + addr
                .hexString() + ") <- mem[r" + rs2 + "], data: " + mirror(rs2).hexString()
            )

            // mask
            dut.io.input.simWrite(
              dut,
              p => {
                MockPayload.create(
                  p,
                  t = 0,
                  rs1 = Some(rs1),
                  rs2 = None,
                  const = Some(wordMask),
                  rd = Some(rs1),
                  opc = GenericOpcode.AND
                )
              }
            )

            dut.io.input.simWrite(
              dut,
              p => {
                MockPayload.create(
                  p,
                  t = 4,
                  rs1 = Some(rs1),
                  rs2 = Some(rs2),
                  const = Some(0),
                  rd = None,
                  opc = GenericOpcode.ST
                )
              }
            )

            expectedExtraWritebackCount += 1
          }
        }
      }

      val expectedTotalWritebacks = testSize + expectedExtraWritebackCount

      while (writebackCount < expectedTotalWritebacks) {
        dut.clockDomain.waitSampling()
      }

      val endCycles = dut.io.cycles.toBigInt
      println(
        "Finished " + testSize + " instructions in " + (endCycles - startCycles) + " cycles. IPC=" + (testSize.toDouble / (endCycles - startCycles).toDouble)
      )
      println(
        "Total inserted delay is " + delayCount + " so minimum possible cycle count is " + (testSize + delayCount) + "."
      )

      dut.clockDomain.waitSampling(100)
      assert(writebackCount == expectedTotalWritebacks)

      for ((k, v) <- caseCount) {
        println(k + ": " + v)
      }

      for (i <- 0 until mspec.numArchitecturalRegs) {
        dut.io.regReadAddr #= i
        dut.clockDomain.waitSampling()
        val data = dut.io.regReadData.toBigInt
        assert(data == mirror(i), "value validation failed for reg " + i)
        println("reg " + i + ": " + data)
      }

      assert(caseCount("DUMMY_EFFECT") == nextDummyEffectValue)
      println("validation ok")
    }
  }
}
