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

  val mspec = MachineSpec(
    numArchitecturalRegs = 8,
    numPhysicalRegs = 64,
    dataWidth = 32 bits,
    maxNumSrcRegsPerInsn = 2,
    maxNumDstRegsPerInsn = 1,
    issueQueueSize = 32,
    functionUnitTagType = HardType(TestTag()),
    robSize = 32,
    commitWidth = 2
  )

  val msem = new MachineSemantics {
    def functionUnits: Seq[FunctionUnit] = Seq(
      new Alu(TestTag.static(0), AluConfig(alu32 = false)),
      new Multiplier(TestTag.static(1), MultiplierConfig())
    )
  }

  object GenericOpcode extends SpinalEnum(binarySequential) {
    val ADD, SUB, AND, OR, XOR, MOV = newElement()

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
    Machine.provide(mspec)
    Machine.provide(msem)
    val pipeline = BackendPipeline(MockPayload())
    val io = new Bundle {
      val input = slave(Stream(MockPayload()))

      val regReadAddr = in(Machine.get[MachineSpec].archRegIndexType)
      val regReadData = out(Machine.get[MachineSpec].dataType)
    }
    io.input >> pipeline.io.input
    val prfIf = Machine.get[PrfInterface]
    io.regReadData := prfIf.readAsync(pipeline.rename.cmt(io.regReadAddr)).data
  }

  test("TestBackendPipeline") {
    SimConfig.withWave.doSim(
      rtl = Machine.build { new TestBackendPipelineTop() },
      name = "test"
    ) { dut =>
      dut.io.input.valid #= false

      dut.clockDomain.forkStimulus(100)
      waitUntil(dut.clockDomain.isResetAsserted)
      waitUntil(dut.clockDomain.isResetDeasserted)

      // Zero out registers
      for (i <- 0 until mspec.numArchitecturalRegs) {
        dut.io.input.simWrite(
          dut,
          p => {
            MockPayload.create(
              p,
              t = 0,
              rs1 = Some(i),
              rs2 = Some(i),
              rd = Some(i),
              opc = GenericOpcode.XOR
            )
          }
        )
      }

      val mirror =
        (0 until mspec.numArchitecturalRegs).map(_ => BigInt(0)).to[ArrayBuffer]

      for (i <- 0 until 1000) {
        val op = Random.nextInt(3)
        op match {
          case 0 =>
            // LD_CONST
            val dst = Random.nextInt(mspec.numArchitecturalRegs)
            val value = Random.nextInt(100000)
            //println("ld_const " + value + " -> r" + dst)
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
          case 1 =>
            // ADD
            val left = Random.nextInt(mspec.numArchitecturalRegs)
            val right = Random.nextInt(mspec.numArchitecturalRegs)
            val dst = Random.nextInt(mspec.numArchitecturalRegs)
            //println("add r" + left + " r" + right + " -> r" + dst)
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
          case 2 =>
            // MUL
            val left = Random.nextInt(mspec.numArchitecturalRegs)
            val right = Random.nextInt(mspec.numArchitecturalRegs)
            val dst = Random.nextInt(mspec.numArchitecturalRegs)
            //println("mul r" + left + " r" + right + " -> r" + dst)
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
        }
      }

      dut.clockDomain.waitSampling(10000)

      for (i <- 0 until mspec.numArchitecturalRegs) {
        dut.io.regReadAddr #= i
        dut.clockDomain.waitSampling()
        val data = dut.io.regReadData.toBigInt
        assert(data == mirror(i), "value validation failed for reg " + i)
      }
      println("validation ok")
    }
  }
}
