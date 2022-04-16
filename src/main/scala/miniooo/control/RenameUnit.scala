package miniooo.control

import spinal.core._
import spinal.lib._
import miniooo.util._

case class RenameInfo(parentObjects: Seq[PolymorphicDataChain]) extends Bundle with PolymorphicDataChain {
  private val spec = Machine.get[MachineSpec]

  val physSrcRegs =
    Vec(spec.physRegIndexType, spec.maxNumSrcRegsPerInsn)
  val physDstRegs =
    Vec(spec.physRegIndexType, spec.maxNumDstRegsPerInsn)
}