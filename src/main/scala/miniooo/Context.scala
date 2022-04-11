package miniooo

import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer

case class MachineContext(
    cfg: MachineConfig,
    sem: MachineSemantics
) {
  val prfContent = RatMem(PrfItem(this), cfg.numPhysicalRegs)
  val prfState =
    Reg(PrfStateTableSnapshot(this)) init (PrfStateTableSnapshot.default(this))
  for (entry <- prfState.table) {
    entry.check()
  }

  val rename = Rename(this, sem.newDecodedInsn)
  val dispatch = Dispatch(this)
  dispatch.io.insnInput << rename.io.output

  val issue = Issue(this)
}
