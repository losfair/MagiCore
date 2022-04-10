package miniooo

import spinal.core._
import spinal.lib._

trait DerefToInsn {
  def parentContext: DerefToInsn
  def insn: DecodedInsn
}
