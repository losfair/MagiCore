package miniooo

import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer
import MiniOoOExt._

case class PrfItem(ctx: MachineContext) extends Bundle {
  val data = Bits(ctx.cfg.dataWidth)
}

object PrfStateTableSnapshot {
  def default(ctx: MachineContext): PrfStateTableSnapshot = {
    val v = PrfStateTableSnapshot(ctx)
    v.table.foreach(_ := PhysRegState.default(ctx))
    v
  }
}

case class PhysRegState(ctx: MachineContext) extends Bundle {
  def busy = Bool()
  def dataAvailable = Bool()
  def allocatable = Bool()
  def check(): this.type = {
    assert(
      (busy && !dataAvailable && !allocatable) // Color: BUSY
        || (!busy && dataAvailable && !allocatable) // Color: DATA_AVAILABLE_NOT_COMMITTED
        || (!busy && dataAvailable && allocatable) // Color: IDLE
    )
    this
  }
}

object PhysRegState {
  def default(ctx: MachineContext): PhysRegState = {
    val st = PhysRegState(ctx)
    st.busy := False
    st.dataAvailable := True
    st.allocatable := True
    st
  }
}

case class PrfStateTableSnapshot(ctx: MachineContext) extends Bundle {
  val table = Vec(PhysRegState(ctx), ctx.cfg.numPhysicalRegs)

  def findFreeReg(): (Bool, UInt) = {
    val (ok, index) = this.table.zipWithIndex
      .map(x => (x._1.allocatable, U(x._2, ctx.cfg.archRegIndexWidth)))
      .reduceBalancedTree((a, b) => {
        val ok = Bool()
        val index = ctx.cfg.physRegIndexType
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

  def markAsBusy(index: UInt): PrfStateTableSnapshot = {
    assert(
      this.table(index).allocatable,
      Seq("Register not allocatable: ", index)
    )
    val snapshot = PrfStateTableSnapshot(ctx)
    snapshot.table := Vec(this.table.zipWithIndex.map(arg => {
      val (x, i) = arg
      val v = PhysRegState(ctx)
      when(index === i) {
        v.busy := True
        v.dataAvailable := False
        v.allocatable := False
      } otherwise {
        v := x
      }
      v
    }))
    snapshot
  }
}
