package magicore.control

import spinal.core._
import spinal.lib._
import scala.reflect._
import magicore.util.PolymorphicDataChain

case class MachineSpec(
    numArchitecturalRegs: Int,
    numPhysicalRegs: Int,
    addrWidth: BitCount,
    dataWidth: BitCount,
    maxNumSrcRegsPerInsn: Int,
    maxNumDstRegsPerInsn: Int,
    issueQueueSize: Int,
    inOrderIssueQueueSize: Int = 16,
    functionUnitTagType: HardType[_ <: Data],
    robSize: Int,
    writebackWidth: Int
) {
  def dataType = Bits(dataWidth)

  def archRegIndexWidth = log2Up(numArchitecturalRegs) bits
  def physRegIndexWidth = log2Up(numPhysicalRegs) bits
  def archRegIndexType = UInt(archRegIndexWidth)
  def physRegIndexType = UInt(physRegIndexWidth)

  def robEntryIndexWidth = log2Up(robSize) bits
  val robEntryIndexType = HardType(UInt(robEntryIndexWidth))

  val numEpochs = 4
  val epochWidth = log2Up(numEpochs) bits
  val epochType = HardType(UInt(epochWidth))

  val epochCounterType = HardType(UInt(8 bits))
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
  def tryGet[T: ClassTag]: Option[T] = current.tryGet[T]

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

  def isDebugEnabled: Boolean = {
    try {
      get[MachineDebugMarker.type]
    } catch {
      case _: Exception => {
        return false
      }
    }

    true
  }

  def debugGen[T <: AnyRef](f: => T): T = {
    if (isDebugEnabled) {
      f
    } else {
      null.asInstanceOf[T]
    }
  }

  def report(data: => Seq[Any]) {
    debugGen {
      spinal.core.report(data)
    }
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
  def tryGet[T: ClassTag]: Option[T] = {
    singletons.get(classTag[T]).map(x => x.asInstanceOf[T])
  }
}

object MachineDebugMarker {}

case class MachineException() extends Bundle {
  private val spec = Machine.get[MachineSpec]
  val valid = Bool()
  val code = MachineExceptionCode()
  val context2 = Bits(spec.addrWidth)
  val context3 = Bool()
  val context4 = Bool()

  def brDstAddr = context2
  def brIsConst = context3
  def brTaken = context4

  def memoryError_accessAddr = context2

  def resetArea[T](f: => T): T = {
    new ResetArea(reset = valid, cumulative = true) {
      val out = f
    }.out
  }
}

object MachineException {
  def idle: MachineException = {
    val e = MachineException()
    e.valid := False
    e.code.assignDontCare()
    e.context2.assignDontCare()
    e.context3.assignDontCare()
    e.context4.assignDontCare()
    e
  }
}

case class FullMachineException(dataType: HardType[_ <: PolymorphicDataChain])
    extends Bundle
    with PolymorphicDataChain {
  val ctx = dataType()
  def exc = ctx.lookup[MachineException]

  def parentObjects = Seq(exc, ctx)
}

object FullMachineException {
  def idle(
      dataType: HardType[_ <: PolymorphicDataChain]
  ): FullMachineException = {
    val e = FullMachineException(dataType)
    e.ctx.assignDontCare()
    e.exc := MachineException.idle
    e
  }
}

object MachineExceptionCode extends SpinalEnum(binarySequential) {
  val BRANCH_MISS, INSN_CACHE_MISS, INSN_MEMORY_ERROR, DECODE_ERROR, DIVIDE_ERROR, SERIALIZE,
      MEMORY_ERROR, INSN_CACHE_FLUSH, EXCEPTION_RETURN, INSN_ALIGNMENT_ERROR,
      LOAD_ALIGNMENT_ERROR, STORE_ALIGNMENT_ERROR, EXT_INTERRUPT, ENV_CALL, WFI,
      RETRY = newElement()

  def shouldSuppressWriteback(
      code: SpinalEnumCraft[MachineExceptionCode.type]
  ): Bool = {
    code.mux(
      BRANCH_MISS -> False,
      SERIALIZE -> False,
      default -> True
    )
  }
}
