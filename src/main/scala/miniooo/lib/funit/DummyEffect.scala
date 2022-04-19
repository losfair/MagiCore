package miniooo.lib.funit

import spinal.core._
import spinal.lib._
import miniooo.util.PolymorphicDataChain
import miniooo.control._
import miniooo.util.MultiLaneFifo

case class DummyEffectOperation() extends Bundle with PolymorphicDataChain {
  def parentObjects = Seq()
  val value = UInt(32 bits)
}

trait DummyEffectInstance extends FunctionUnitInstance {
  def effectOutput: Flow[UInt]
}

class DummyEffect(val staticTag: Data) extends FunctionUnit {
  private val spec = Machine.get[MachineSpec]

  private var effInst: EffectInstance = null

  override def inOrder: Boolean = true

  override def generate(
      hardType: HardType[_ <: PolymorphicDataChain]
  ): FunctionUnitInstance = {
    new DummyEffectInstance {

      val io_available: Bool = null
      val io_input = Stream(hardType())
      val io_output = Stream(CommitRequest(null))
      val effectOutput = Flow(UInt(32 bits))

      val pending = Mem(UInt(32 bits), spec.robSize)
      val pendingValid = Vec(Reg(Bool()) init (false), spec.robSize)

      val in = io_input.payload
      val op = in.lookup[DummyEffectOperation]
      val dispatch = in.lookup[DispatchInfo]
      val issue = in.lookup[IssuePort[_]]

      val out = CommitRequest(null)
      out.token := dispatch.lookup[CommitToken]
      out.exception := MachineException.idle
      out.regWriteValue.assignDontCare()

      io_output << io_input
        .translateWith(out)
        .continueWhen(!pendingValid(dispatch.robIndex))
      pending.write(
        address = dispatch.robIndex,
        data = op.value,
        enable = io_input.fire
      )
      when(io_input.fire) {
        pendingValid(dispatch.robIndex) := True
      }

      val effFifo = MultiLaneFifo(
        dataType = CommitEffect(),
        depth = spec.robSize,
        numLanes = spec.commitWidth
      )
      assert(!effFifo.io.push.isStall, "effect fifo must not stall")

      effFifo.io.push.valid := effInst.io_effect.map(x => x.valid).orR
      for ((eff, i) <- effInst.io_effect.zipWithIndex) {
        effFifo.io.push.payload(i).valid := eff.valid
        effFifo.io.push.payload(i).robIndex := eff.payload.robIndex
      }

      effectOutput.setIdle()

      // Create latency
      val pop = effFifo.io.pop.stage().stage().stage()
      pop.setBlocked()
      when(pop.valid) {
        assert(pendingValid(pop.payload.robIndex), "pendingValid must be true")
        pop.ready := True
        pendingValid(pop.payload.robIndex) := False
        val value = pending(pop.payload.robIndex)
        Machine.report(Seq("dummy effect: ", value))
        effectOutput.valid := True
        effectOutput.payload := value
      }
    }
  }

  override def generateEffect(): Option[EffectInstance] = {
    val spec = Machine.get[MachineSpec]
    effInst = new EffectInstance {
      val io_effect: Vec[Flow[CommitEffect]] =
        Vec(Flow(CommitEffect()), spec.commitWidth)
      val io_reset: Bool = Bool()
    }
    Some(effInst)
  }

}
