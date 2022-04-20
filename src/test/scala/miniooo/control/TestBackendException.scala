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

class TestBackendException extends AnyFunSuite {
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
    def functionUnits: Seq[FunctionUnit] = Seq(
      new Alu(TestTag.static(0), AluConfig(alu32 = false)),
      new Multiplier(TestTag.static(1), MultiplierConfig()),
      new Divider(TestTag.static(2), enableException = true)
    )
  }

  object GenericOpcode extends SpinalEnum(binarySequential) {
    val ADD, SUB, AND, OR, XOR, MOV, DIV_S, DIV_U, REM_S, REM_U = newElement()

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

  class TestBackendExceptionTop extends Component {
    Machine.provide(MachineDebugMarker)
    Machine.provide(mspec)
    Machine.provide(msem)
    val pipeline = BackendPipeline(MockPayload())
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
      val exception = out(MachineException())
    }
    io.exception := pipeline.exception
    io.input >> pipeline.io.input
    val prfIf = Machine.get[PrfInterface]
    io.regReadData := prfIf.readAsync(pipeline.rename.cmt(io.regReadAddr)).data
    pipeline.io.writebackMonitor
      .zip(io.writebackMonitor)
      .foreach(x => x._1 >> x._2)

    val cycles = Reg(UInt(64 bits)) init (0)
    cycles := cycles + 1

    io.cycles := cycles
  }

  test("TestBackendException") {
    SimConfig.withWave.doSim(
      rtl = Machine.build { new TestBackendExceptionTop() },
      name = "test"
    ) { dut =>
      dut.io.input.valid #= false

      dut.clockDomain.forkStimulus(100)
      waitUntil(dut.clockDomain.isResetAsserted)
      waitUntil(dut.clockDomain.isResetDeasserted)

      dut.clockDomain.waitSampling(100) // wait for preparation

      // Init data
      dut.io.input.simWrite(
        dut,
        p => {
          MockPayload.create(
            p,
            t = 0,
            rs1 = None,
            rs2 = None,
            const = Some(33),
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
            const = Some(6),
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
            t = 0,
            rs1 = None,
            rs2 = None,
            const = Some(0),
            rd = Some(2),
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
            const = Some(0),
            rd = Some(3),
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
            const = Some(0),
            rd = Some(4),
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
            const = Some(0),
            rd = Some(5),
            opc = GenericOpcode.MOV
          )
        }
      )

      val cyc1 = dut.io.cycles.toBigInt

      // Inject long-latency operations
      dut.io.input.simWrite(
        dut,
        p => {
          MockPayload.create(
            p,
            t = 1,
            rs1 = Some(0),
            rs2 = Some(1),
            const = None,
            rd = Some(3), // 198
            opc = GenericOpcode.ADD // mul actually
          )
        }
      )
      dut.io.input.simWrite(
        dut,
        p => {
          MockPayload.create(
            p,
            t = 2,
            rs1 = Some(3),
            rs2 = Some(2),
            const = None,
            rd = Some(4), // ERROR
            opc = GenericOpcode.DIV_U
          )
        }
      )

      // These will not be committed
      dut.io.input.simWrite(
        dut,
        p => {
          MockPayload.create(
            p,
            t = 0,
            rs1 = Some(0),
            rs2 = Some(1),
            const = None,
            rd = Some(5),
            opc = GenericOpcode.ADD
          )
        }
      )
      dut.io.input.simWrite(
        dut,
        p => {
          MockPayload.create(
            p,
            t = 2,
            rs1 = Some(3),
            rs2 = Some(2),
            const = None,
            rd = Some(4),
            opc = GenericOpcode.DIV_U
          )
        }
      )

      // wait for exception
      dut.clockDomain.waitSamplingWhere(dut.io.exception.valid.toBoolean)
      val cyc2 = dut.io.cycles.toBigInt
      assert(dut.io.exception.code.toEnum == MachineExceptionCode.DIVIDE_ERROR)
      println(
        "got exception " + dut.io.exception.code.toEnum + " after " + (cyc2 - cyc1) + " cycles"
      )
      dut.clockDomain.waitSampling()
      assert(dut.io.exception.valid.toBoolean == false)

      // Ensure no further exception
      fork {
        while (true) {
          dut.clockDomain.waitSampling()
          assert(dut.io.exception.valid.toBoolean == false)
        }
      }

      dut.io.input.simWrite(
        dut,
        p => {
          MockPayload.create(
            p,
            t = 0,
            rs1 = Some(0),
            rs2 = Some(1),
            const = None,
            rd = Some(6),
            opc = GenericOpcode.ADD
          )
        }
      )
      dut.clockDomain.waitSampling(100)

      val expected: Seq[(Int, BigInt)] =
        Seq((0, 33), (1, 6), (2, 0), (3, 198), (4, 0), (5, 0), (6, 39))
      for ((index, value) <- expected) {
        dut.io.regReadAddr #= index
        dut.clockDomain.waitSampling()
        assert(dut.io.regReadData.toBigInt == value)
      }
      println("validation ok")
    }
  }
}
