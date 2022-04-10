package miniooo

import spinal.core._
import spinal.lib._

case class MachineConfig(
    insnWidth: BitCount,
    maxNumSrcRegsPerInsn: Int,
    maxNumDstRegsPerInsn: Int,
    numArchitecturalRegs: Int,
    numPhysicalRegs: Int,
    dataWidth: BitCount
) {
  def archRegIndexWidth = log2Up(numArchitecturalRegs) bits
  def physRegIndexWidth = log2Up(numPhysicalRegs) bits

  def archRegIndexType = UInt(archRegIndexWidth)
  def physRegIndexType = UInt(physRegIndexWidth)
  def dataType = Bits(dataWidth)
}

abstract class MachineSemantics {
  def newDecodedInsn(): DecodedInsn
  def functionUnits: Seq[FunctionUnit]
}

case class FunctionUnitContext(
    cfg: MachineConfig,
    parentContextType: () => Bundle with DerefToInsn
) {
  val input = Stream(parentContextType())
  val output = Stream(CommitRequest(cfg, parentContextType))
}

case class CommitRequest(
    cfg: MachineConfig,
    parentContextType: () => Bundle with DerefToInsn
) extends Bundle {
  val parentContext = parentContextType()
  val regWriteValue = cfg.dataType
}

abstract class FunctionUnit {
  def tag: Data
  def singleCycleBypassable: Boolean
  def generate(ctx: FunctionUnitContext): Unit
}

case class MaybeArchRegIndex(cfg: MachineConfig) extends Bundle {
  val valid = Bool()
  val index = cfg.archRegIndexType
}

abstract class DecodedInsn(cfg: MachineConfig) extends Bundle with DerefToInsn {
  def insn: DecodedInsn = this
  def parentContext: DecodedInsn = this
  def archSrcRegs: Vec[MaybeArchRegIndex]
  def archDstRegs: Vec[MaybeArchRegIndex]
  def functionUnitTag: Data
}
