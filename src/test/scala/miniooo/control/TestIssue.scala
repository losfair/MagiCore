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

class TestIssue extends AnyFunSuite {
  val mspec = MachineSpec(
    numArchitecturalRegs = 4,
    numPhysicalRegs = 64,
    dataWidth = 32 bits,
    maxNumSrcRegsPerInsn = 2,
    maxNumDstRegsPerInsn = 1,
    issueQueueSize = 32,
    functionUnitTagType = HardType(TestTag())
  )
  val numFunctionUnits = 4

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

  case class TestPayload() extends Bundle with PolymorphicDataChain {
    val rename = RenameInfo(null)
    val decode = DecodeInfo(null)
    def parentObjects = Seq(rename, decode)
  }

  case class TestRegWriteRequest() extends Bundle {
    val physRegIndex = Machine.get[MachineSpec].physRegIndexType
    val physRegData = Machine.get[MachineSpec].dataType
    val write = Bool()
    val wake = Bool()
    val stall = Bool()
  }

  class IssueTop extends Component {
    Machine.provide(mspec)

    val prf = PrfUnit()
    Machine.provide(prf.interface)

    val issue = IssueUnit(
      c = IssueConfig(portSpecs =
        (0 until numFunctionUnits).map(i =>
          IssueSpec(staticTag = TestTag.static(i))
        )
      ),
      dataType = HardType(TestPayload())
    )

    val prfIf = Machine.get[PrfInterface]

    val io = new Bundle {
      val input = slave(Stream(TestPayload()))
      val issuePorts = Vec(
        master Stream (IssuePort(HardType(TestPayload()))),
        issue.c.portSpecs.size
      )
      val regWritePorts = Vec(
        slave Stream (TestRegWriteRequest()),
        issue.c.portSpecs.size
      )
      val simDispatchWrite = slave Stream (TestRegWriteRequest())

      val regReadAddr = in(Machine.get[MachineSpec].physRegIndexType)
      val regReadData = out(Machine.get[MachineSpec].dataType)
    }

    io.regReadData := prfIf.readAsync(io.regReadAddr).data

    val simRegWrite = StreamArbiterFactory.roundRobin.on(
      io.regWritePorts.toSeq ++ Seq(io.simDispatchWrite)
    )

    issue.io.input << io.input
    issue.io.issuePorts
      .zip(io.issuePorts)
      .foreach({ case (src, dst) => src >> dst })

    val prfIfWriteData = PrfItem()

    simRegWrite.freeRun()
    prfIfWriteData.data := simRegWrite.payload.physRegData
    prfIf.write(
      enable = simRegWrite.valid && simRegWrite.payload.write,
      address = simRegWrite.payload.physRegIndex,
      data = prfIfWriteData
    )
    prfIf.notify(
      enable = simRegWrite.valid && simRegWrite.payload.wake,
      index = simRegWrite.payload.physRegIndex
    )
    when(simRegWrite.valid && simRegWrite.payload.wake) {
      val slot = prfIf.state.table(simRegWrite.payload.physRegIndex)
      spinal.core.assert(
        slot.busy && !slot.dataAvailable,
        Seq(
          "wake precondition failed on slot ",
          simRegWrite.payload.physRegIndex,
          ": ",
          slot.busy,
          " ",
          slot.dataAvailable
        )
      )
      slot.busy := False
      slot.dataAvailable := True
    }
    when(simRegWrite.valid && simRegWrite.payload.stall) {
      val slot = prfIf.state.table(simRegWrite.payload.physRegIndex)
      spinal.core.assert(
        !slot.busy && slot.dataAvailable,
        Seq(
          "stall precondition failed on slot ",
          simRegWrite.payload.physRegIndex,
          ": ",
          slot.busy,
          " ",
          slot.dataAvailable
        )
      )
      slot.busy := True
      slot.dataAvailable := False
    }
  }
  test("TestIssue") {
    SimConfig.withWave.doSim(
      rtl = Machine.build { new IssueTop() }
    ) { dut =>
      def genRegWriteReq(
          out: TestRegWriteRequest,
          write: Boolean,
          wake: Boolean,
          stall: Boolean,
          index: Int,
          data: BigInt
      ) {
        out.write #= write
        out.wake #= wake
        out.stall #= stall
        out.physRegIndex #= index
        out.physRegData #= data
      }

      dut.io.input.valid #= false
      dut.io.issuePorts.foreach(x => x.ready #= false)
      dut.io.regWritePorts.foreach(x => x.valid #= false)
      dut.io.simDispatchWrite.valid #= false

      dut.clockDomain.forkStimulus(100)
      waitUntil(dut.clockDomain.isResetAsserted)
      waitUntil(dut.clockDomain.isResetDeasserted)

      var cycles = BigInt(0)
      fork {
        while (true) {
          dut.clockDomain.waitSampling()
          cycles += 1
        }
      }

      val writableRegs = Random
        .shuffle((1 until mspec.numPhysicalRegs).map(x => x))
        .to[ArrayBuffer]
      val readRefCount =
        (0 until mspec.numPhysicalRegs).map(_ => 0).to[ArrayBuffer]

      val regContentMirror =
        (0 until mspec.numPhysicalRegs)
          .map(_ => Random.nextInt(100000))
          .toBuffer

      // Initialize registers
      for (index <- 0 until mspec.numPhysicalRegs) {
        val p = dut.io.regWritePorts(0)
        p.simWrite(
          dut,
          p =>
            genRegWriteReq(
              out = p,
              write = true,
              wake = false,
              stall = false,
              index = index,
              data = regContentMirror(index)
            )
        )
      }

      println("Initialized " + writableRegs.size + " registers.")

      for (i <- 0 until numFunctionUnits) {
        val fuIndex = i
        val port = dut.io.issuePorts(i)
        val regWritePort = dut.io.regWritePorts(i)
        var inputActive = false
        var outputActive = false

        val doTxn = () => {
          assert(
            port.valid.toBoolean,
            "valid signal disappeared on fu " + fuIndex
          )

          /*println(
            "fu " + fuIndex + " got work dstReg=" + port.payload
              .lookup[RenameInfo]
              .physDstRegs(0)
              .toInt + " cyc " + cycles
          )*/

          val payload = port.payload
          val renameInfo = payload.lookup[RenameInfo]
          val decodeInfo = payload.lookup[DecodeInfo]
          val srcRegIndex = renameInfo.physSrcRegs(0).toInt
          val dstRegIndex = renameInfo.physDstRegs(0).toInt
          val outputData = payload.srcRegData(0).toBigInt + 1
          val pipelineDelay = Random.nextInt(50)

          fork {
            if (pipelineDelay != 0) {
              dut.clockDomain.waitSampling(pipelineDelay)
            }

            // XXX: Are the simulation "threads" really OS threads?
            // Need to use sync primitives if so.
            while (outputActive) {
              dut.clockDomain.waitSampling()
            }

            outputActive = true
            regWritePort.simWrite(
              dut,
              p =>
                genRegWriteReq(
                  out = p,
                  write = true,
                  wake = true,
                  stall = false,
                  index = dstRegIndex,
                  data = outputData
                )
            )
            writableRegs += dstRegIndex
            assert(readRefCount(srcRegIndex) > 0)
            readRefCount.update(srcRegIndex, readRefCount(srcRegIndex) - 1)
            outputActive = false
            /*println(
              "Function unit " + fuIndex + " wrote to register " + dstRegIndex + " value " + outputData + " src " + srcRegIndex + " " + writableRegs
            )*/
          }
        }

        val inputLatency = Random.nextInt(10)
        println(
          "simulated input latency for function unit " + fuIndex + " is " + inputLatency
        )
        port.simContinuousRead(
          dut,
          inputLatency,
          (_) => {
            doTxn()
          }
        )
      }

      for (i <- 0 until 1000) {
        var maybeDstRegIndex: Option[Int] = None
        while (maybeDstRegIndex.isEmpty) {
          val it =
            writableRegs.zipWithIndex.find(arg => readRefCount(arg._1) == 0)
          it match {
            case Some((regIndex, arrayIndex)) => {
              writableRegs.remove(arrayIndex)
              maybeDstRegIndex = Some(regIndex)
            }
            case None => {
              dut.clockDomain.waitSampling()
            }
          }
        }
        val dstRegIndex = maybeDstRegIndex.get
        dut.clockDomain.waitSampling(Random.nextInt(10))
        var srcRegIndex = Random.nextInt(dstRegIndex)
        readRefCount.update(srcRegIndex, readRefCount(srcRegIndex) + 1)
        val funcUnit = Random.nextInt(numFunctionUnits)
        /*println(
          "selected fu=" + funcUnit + " src=" + srcRegIndex + "(" + regContentMirror(
            srcRegIndex
          ) + ") dst=" + dstRegIndex + "(" + regContentMirror(dstRegIndex) + ")"
        )*/

        dut.io.simDispatchWrite.simWrite(
          dut,
          p =>
            genRegWriteReq(
              out = p,
              write = false,
              wake = false,
              stall = true,
              index = dstRegIndex,
              data = 0
            )
        )

        dut.io.input.simWrite(
          dut,
          p => {
            p.rename.physDstRegs(0) #= dstRegIndex
            p.rename.physSrcRegs(0) #= srcRegIndex
            p.decode.functionUnitTag
              .asInstanceOf[TestTag]
              .tag #= funcUnit
            p.decode.archDstRegs(0).valid #= true
            p.decode.archSrcRegs(0).valid #= true
            p.decode.archSrcRegs(1).valid #= false
          }
        )
        regContentMirror.update(dstRegIndex, regContentMirror(srcRegIndex) + 1)
      }
      println("waiting for completion")
      dut.clockDomain.waitSampling(1000) // XXX: Figure this out dynamically?

      val actual = (0 until mspec.numPhysicalRegs).map(i => {
        dut.io.regReadAddr #= i
        dut.clockDomain.waitSampling()
        dut.io.regReadData.toBigInt
      })

      //println("actual: " + actual)
      //println("expected: " + regContentMirror)

      for (i <- 0 until mspec.numPhysicalRegs) {
        assert(
          actual(i) == regContentMirror(i),
          "register " + i + " has wrong value"
        )
      }
      println("check passed: " + regContentMirror)
    }
  }
}
