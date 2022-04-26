package magicore.lib.funit

import spinal.core._
import spinal.lib._
import magicore.util.PolymorphicDataChain
import magicore.control._
import magicore.util.MultiLaneFifo

case class DummyEffectOperation() extends Bundle with PolymorphicDataChain {
  def parentObjects = Seq()
  val value = UInt(32 bits)
}

trait DummyEffectInstance extends FunctionUnitInstance {
  def effectOutput: Flow[UInt]
}

class DummyEffect(staticTagData: => Data, inOrder_sideEffect_enable: Boolean)
    extends FunctionUnit {
  def staticTag: Data = staticTagData
  private val spec = Machine.get[MachineSpec]

  private var effInst: EffectInstance = null

  override def inOrder_sideEffect: Boolean = inOrder_sideEffect_enable

  override def inOrder: Boolean = true

  override def generate(
      hardType: HardType[_ <: PolymorphicDataChain]
  ): FunctionUnitInstance = {
    new DummyEffectInstance {

      val io_available: Bool = null
      val io_input = Stream(hardType())
      val io_output = Stream(CommitRequest(null))
      val effectOutput = Flow(UInt(32 bits))

      val in = io_input.payload
      val op = in.lookup[DummyEffectOperation]
      val dispatch = in.lookup[DispatchInfo]
      val issue = in.lookup[IssuePort[_]]

      val out = CommitRequest(null)
      out.token := dispatch.lookup[CommitToken]
      out.exception := MachineException.idle
      out.regWriteValue(0) := (issue.srcRegData(0).asUInt + 42).asBits

      if(inOrder_sideEffect) {
        io_output << io_input
          .translateWith(out)
        effectOutput.setIdle()
        when(io_output.fire) {
          effectOutput.valid := True
          effectOutput.payload := op.value
        }
      } else {
        val pending = Mem(UInt(32 bits), spec.robSize)
        val pendingValid_scheduled = Machine.get[MachineException].resetArea {
          Vec(Reg(Bool()) init (false), spec.robSize)
        }
        val pendingValid_posted = Vec(Reg(Bool()) init (false), spec.robSize)
        val pendingValid = Vec(
          pendingValid_scheduled.zip(pendingValid_posted).map(x => x._1 || x._2)
        )

        io_output << io_input
          .translateWith(out)
          .continueWhen(!pendingValid(dispatch.robIndex))
        pending.write(
          address = dispatch.robIndex,
          data = op.value,
          enable = io_input.fire
        )
        when(io_input.fire) {
          pendingValid_scheduled(dispatch.robIndex) := True
          Machine.report(
            Seq(
              "scheduled dummy effect with robIndex ",
              dispatch.robIndex,
              " value ",
              op.value
            )
          )
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
          effFifo.io.push.payload(i).payload := eff.payload.refine()
        }

        when(effFifo.io.push.fire) {
          for (eff <- effInst.io_effect) {
            when(eff.valid) {
              assert(
                pendingValid_scheduled(
                  eff.payload.robIndex
                ) === True,
                "dummy effect not scheduled"
              )
              pendingValid_scheduled(
                eff.payload.robIndex
              ) := False
              pendingValid_posted(eff.payload.robIndex) := True
              Machine.report(
                Seq("posted dummy effect with robIndex ", eff.payload.robIndex)
              )
            }
          }
        }

        effectOutput.setIdle()

        // Create latency
        val pop = effFifo.io.pop.stage().stage().stage()
        pop.setBlocked()
        when(pop.valid) {
          assert(
            pendingValid_posted(pop.payload.robIndex),
            "pendingValid_posted must be true"
          )
          pop.ready := True
          pendingValid_posted(pop.payload.robIndex) := False
          val value = pending(pop.payload.robIndex)
          Machine.report(
            Seq("dummy effect: ", value, " robIndex ", pop.payload.robIndex)
          )
          effectOutput.valid := True
          effectOutput.payload := value
        }
      }
    }
  }

  override def generateEffect(): Option[EffectInstance] = {
    val spec = Machine.get[MachineSpec]
    effInst = new EffectInstance {}
    Some(effInst)
  }

}
