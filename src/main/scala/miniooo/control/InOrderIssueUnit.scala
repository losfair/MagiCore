package miniooo.control

import spinal.core._
import spinal.lib._
import miniooo.util._
import MiniOoOExt._
import scala.reflect._

case class InOrderIssueUnit[T <: PolymorphicDataChain](
    c: IssueConfig,
    dataType: HardType[T],
    reset: Bool
) extends Area {
  private val spec = Machine.get[MachineSpec]
  private val prf = Machine.get[PrfInterface]

  case class IqData() extends Bundle {
    val data = dataType()
  }

  def issueDataType = IssuePort(dataType)

  val io = new Bundle {
    val input = Stream(dataType())
    val issuePorts = Vec(Stream(issueDataType), c.portSpecs.size)
    val issueMonitor = Flow(issueDataType)
  }

  val resetArea = new ResetArea(reset = reset, cumulative = true) {
    val q = new StreamFifoLowLatency(
      dataType = IqData(),
      depth = spec.inOrderIssueQueueSize,
      latency = 1
    )
  }

  val qPush = IqData()
  qPush.data := io.input.payload
  io.input.translateWith(qPush) >> resetArea.q.io.push

  val front = resetArea.q.io.pop.payload
  val dispatchInfo = front.data.lookup[DispatchInfo]
  val decodeInfo = front.data.lookup[DecodeInfo]
  val renameInfo = front.data.lookup[RenameInfo]

  val dataReady = decodeInfo.archSrcRegs
    .zip(renameInfo.physSrcRegs)
    .map({ case (arch, phys) =>
      !arch.valid || prf.state.table(phys).dataAvailable
    })
    .andR

  val issuePort = issueDataType
  for ((r, i) <- issuePort.srcRegData.zipWithIndex) {
    r := prf.readAsync(renameInfo.physSrcRegs(i)).data
  }

  issuePort.data := front.data

  val popStream =
    resetArea.q.io.pop.continueWhen(dataReady).translateWith(issuePort)
  popStream.setBlocked()

  val issueOk = Vec(Bool(), io.issuePorts.size)
  for (b <- issueOk) b := False

  val issueFuIndex = UInt(log2Up(c.portSpecs.size) bits)
  issueFuIndex.assignDontCare()
  for ((fu, fuIndex) <- io.issuePorts.zipWithIndex) {
    fu.valid := False
    fu.payload := popStream.payload

    when(
      decodeInfo.functionUnitTag === c.portSpecs(fuIndex).staticTag
    ) {
      issueFuIndex := fuIndex
      fu.valid := popStream.valid
      popStream.ready := fu.ready
      issueOk(fuIndex) := True
    }
  }

  when(popStream.valid) {
    assert(issueOk.countForVerification(x => x) === 1, "issue count mismatch")
  }
  io.issueMonitor << popStream.asFlow.throwWhen(
    !popStream.ready
  )

  when(popStream.isStall) {
    Machine.report(
      Seq(
        "ioiq stall - rob index ",
        dispatchInfo.robIndex,
        " fuIndex=", issueFuIndex,
        " deps"
      ) ++ decodeInfo.archSrcRegs
        .zip(renameInfo.physSrcRegs)
        .flatMap({ case (arch, phys) =>
          Seq("[v=", arch.valid, " arch=", arch.index, " phys=", phys, "]")
        })
    )
  }

  when(popStream.fire) {
    Machine.report(
      Seq(
        "ioiq issue - rob index ",
        dispatchInfo.robIndex,
        " fuIndex=", issueFuIndex,
        " deps"
      ) ++ decodeInfo.archSrcRegs
        .zip(renameInfo.physSrcRegs)
        .flatMap({ case (arch, phys) =>
          Seq("[v=", arch.valid, " arch=", arch.index, " phys=", phys, "]")
        })
    )
    val epochMgr = Machine.get[EpochManager]
    epochMgr.incReq_ino.valid := True
    epochMgr.incReq_ino.payload := dispatchInfo.epoch

    Machine.report(
      Seq(
        "epoch count inc [ino]: epoch ",
        dispatchInfo.epoch,
        " prev ",
        epochMgr.epochTable(dispatchInfo.epoch)
      )
    )
  }
}
