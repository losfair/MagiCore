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
    functionUnitTagType: HardType[_ <: Data]
) {
  def dataType = Bits(dataWidth)

  def archRegIndexWidth = log2Up(numArchitecturalRegs) bits
  def physRegIndexWidth = log2Up(numPhysicalRegs) bits
  def archRegIndexType = UInt(archRegIndexWidth)
  def physRegIndexType = UInt(physRegIndexWidth)
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
}

class Machine {
  private val singletons = scala.collection.mutable.Map[ClassTag[_], Any]()
  def provide[T: ClassTag](value: T) {
    singletons(classTag[T]) = value
  }

  def get[T: ClassTag]: T = {
    singletons.get(classTag[T]).get.asInstanceOf[T]
  }

}