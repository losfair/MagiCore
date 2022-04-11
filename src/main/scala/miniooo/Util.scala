package miniooo

import spinal.core._
import spinal.lib._

trait DerefToInsn {
  def parentContext: DerefToInsn
  def insn: DecodedInsn

  def chainLookup[T](): T = {
    try {
      return this.asInstanceOf[T]
    } catch {
      case _: ClassCastException => {}
    }
    if (this.parentContext eq this) {
      throw new Exception("did not find the requested type in the chain")
    }
    this.parentContext.chainLookup()
  }
}
