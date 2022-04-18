package miniooo.control

import spinal.core._
import spinal.lib._
import scala.reflect._

case class MachineSpec(
    numArchitecturalRegs: Int,
    numPhysicalRegs: Int,
    dataWidth: BitCount,
    maxNumSrcRegsPerInsn: Int,
    maxNumDstRegsPerInsn: Int,
    issueQueueSize: Int,
    functionUnitTagType: HardType[_ <: Data],
    robSize: Int,
    commitWidth: Int
) {
  def dataType = Bits(dataWidth)

  def archRegIndexWidth = log2Up(numArchitecturalRegs) bits
  def physRegIndexWidth = log2Up(numPhysicalRegs) bits
  def archRegIndexType = UInt(archRegIndexWidth)
  def physRegIndexType = UInt(physRegIndexWidth)

  def robEntryIndexWidth = log2Up(robSize) bits
  val robEntryIndexType = HardType(UInt(robEntryIndexWidth))

  def epochWidth = 16 bits
  val epochType = HardType(UInt(epochWidth))
}

abstract class MachineSemantics {
  def functionUnits: Seq[FunctionUnit]
  def numFunctionUnits = functionUnits.size
}

object Machine {
  private[control] val _current: ThreadLocal[Machine] = new ThreadLocal()
  def current = _current.get()

  def provide[T: ClassTag](value: T) = current.provide(value)
  def get[T: ClassTag]: T = current.get[T]

  def build[T](f: => T): T = {
    assert(Machine._current.get() == null)
    val m = new Machine()
    Machine._current.set(m)
    try {
      f
    } finally {
      Machine._current.set(null)
    }
  }

  def report(data: Seq[Any]) {
    try {
      get[MachineDebugMarker.type]
    } catch {
      case _: Exception => {
        return
      }
    }
    spinal.core.report(data)
  }
}

class Machine {
  private val singletons = scala.collection.mutable.Map[ClassTag[_], Any]()
  def provide[T: ClassTag](value: T) {
    if (singletons.contains(classTag[T])) {
      throw new Exception(s"${classTag[T]} is already provided")
    }
    singletons(classTag[T]) = value
  }

  def get[T: ClassTag]: T = {
    singletons.get(classTag[T]).get.asInstanceOf[T]
  }

}

object MachineDebugMarker {}
