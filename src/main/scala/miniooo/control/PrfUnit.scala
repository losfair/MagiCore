package miniooo.control

import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer
import miniooo.util._
import miniooo.util.MiniOoOExt._

case class PrfItem() extends Bundle {
  val data = Machine.get[MachineSpec].dataType
}

case class PhysRegState() extends Bundle {
  val busy = Bool()
  val dataAvailable = Bool()
  val allocatable = Bool()
  def check(): this.type = {
    assert(
      (busy && !dataAvailable && !allocatable) // Color: BUSY
        || (!busy && dataAvailable && !allocatable) // Color: DATA_AVAILABLE_NOT_COMMITTED
        || (!busy && dataAvailable && allocatable) // Color: IDLE
    )
    this
  }

  def reportSeq(): Seq[Any] = {
    Seq(
      "busy=",
      busy,
      " dataAvailable=",
      dataAvailable,
      " allocatable=",
      allocatable
    )
  }
}

object PhysRegState {
  def default: PhysRegState = {
    val st = PhysRegState()
    st.busy := False
    st.dataAvailable := True
    st.allocatable := True
    st
  }
}

case class PrfStateTableSnapshot(registered: Boolean = false) extends Bundle {
  val spec = Machine.get[MachineSpec]
  val table = Vec(
    if (registered) Reg(PhysRegState()) init (PhysRegState.default)
    else PhysRegState(),
    spec.numPhysicalRegs
  )

  def findFreeReg(allowMask: Vec[Bool]): (Bool, UInt) = {
    val (ok, index) = this.table.zipWithIndex
      .drop(1) // do not allocate the zeroth register
      .map(x =>
        (x._1.allocatable & allowMask(x._2), U(x._2, spec.physRegIndexWidth))
      )
      .reduceBalancedTree((a, b) => {
        val ok = Bool()
        val index = spec.physRegIndexType
        when(a._1) {
          ok := True
          index := a._2
        } elsewhen (b._1) {
          ok := True
          index := b._2
        } otherwise {
          ok := False
          index.assignDontCare()
        }
        (ok, index)
      })
    (ok, index)
  }

  def markAsBusyInPlace(index: UInt) {
    assert(
      this.table(index).allocatable,
      Seq("Register not allocatable: ", index)
    )
    val entry = this.table(index)
    entry.busy := True
    entry.dataAvailable := False
    entry.allocatable := False
  }
}

case class PrfUnit(reset: Bool) extends Area {
  val listeners = new ArrayBuffer[(UInt, Bool)]() // (index, wakeUp)
  val notifiers = new ArrayBuffer[(Bool, UInt)]() // (enable, index)
  val content = LvtMem(PrfItem(), Machine.get[MachineSpec].numPhysicalRegs)
  val state = new ResetArea(reset = reset, cumulative = true) {
    val v = PrfStateTableSnapshot(registered = true)
  }.v

  def interface = PrfInterface(this)

  Component.current.afterElaboration {
    for ((listenerIndex, wakeUp) <- listeners) {
      wakeUp := False

      for ((enable, notifierIndex) <- notifiers) {
        when(enable && notifierIndex === listenerIndex) {
          wakeUp := True
        }
      }
    }

    // Verification
    {
      val dataWasAvailable = new ResetArea(reset = reset, cumulative = true) {
        val v =
          Vec(Reg(Bool()) init (True), Machine.get[MachineSpec].numPhysicalRegs)
      }.v
      for (
        (src, dst) <- state.table.map(_.dataAvailable).zip(dataWasAvailable)
      ) {
        dst := src
      }

      val rising = dataWasAvailable
        .zip(state.table.map(_.dataAvailable))
        .map(x => !x._1 && x._2)
      for ((isRising, i) <- rising.zipWithIndex) {
        val notifyCount = Vec(
          notifiers
            .map(x => x._1 && x._2 === i)
        )
          .countForVerification(x => x)
        assert(
          (isRising && notifyCount === 1) || (!isRising && notifyCount === 0),
          "notify/data mismatch"
        )
      }
    }
  }
}

case class PrfWriteReq() extends Bundle {
  private val spec = Machine.get[MachineSpec]
  val address = spec.physRegIndexType
  val data = PrfItem()
}

case class PrfInterface(unit: PrfUnit) {
  def state = unit.state
  def readAsync(address: UInt) = unit.content.readAsync(address)
  def write(address: UInt, data: PrfItem, enable: Bool) =
    unit.content.write(address = address, data = data, enable = enable)
  def generateWritePort: Flow[PrfWriteReq] = {
    val s = Flow(PrfWriteReq())
    unit.content.write(
      address = s.payload.address,
      data = s.payload.data,
      enable = s.valid
    )
    s
  }

  def listen(index: UInt): Bool = {
    val wakeUp = Bool()
    unit.listeners += ((index, wakeUp))
    wakeUp
  }

  def notify_callerHandlesReset(enable: Bool, index: UInt): Unit = {
    unit.notifiers += ((RegNext(next = enable, init = False), RegNext(index)))

    /*when(enable) {
      val s: Seq[Any] =
        Seq("prf state matrix:") ++ state.table.zipWithIndex.flatMap({ case (x, i) =>
          Seq("\\n\\t", i.toString(), ": ") ++ x.reportSeq()
        })
      Machine.report(s)
    }*/
  }
}
