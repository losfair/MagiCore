package miniooo.lib.funit

import spinal.core._
import spinal.lib._
import miniooo.util.PolymorphicDataChain
import miniooo.control._
import spinal.lib.fsm._

case class DividerOperation() extends Bundle with PolymorphicDataChain {
  def parentObjects = Seq()

  val useRemainder = Bool()
  val signed = Bool()
}

final case class Divider(staticTag: Data) extends FunctionUnit {
  override def warnOnBlockedIssue = true
  override def generate(
      hardType: HardType[_ <: PolymorphicDataChain]
  ): FunctionUnitInstance = {
    new FunctionUnitInstance {
      private val spec = Machine.get[MachineSpec]

      case class RtCtx() extends Bundle {
        val op = DividerOperation()
        val aNeg = Bool()
        val bNeg = Bool()
        val dividend = spec.dataType
        val divisor = spec.dataType
        val quotient = spec.dataType
        val remainder = spec.dataType
        val counter = UInt(log2Up(spec.dataWidth.value) bits)
        val token = CommitToken()
      }

      val io_input = Stream(hardType())
      val io_output = Stream(CommitRequest(null))

      val buffered_input = io_input.s2mPipe()

      val buffered_count = Reg(UInt(3 bits)) init(0)
      when(io_input.fire && !io_output.fire) {
        assert(buffered_count =/= buffered_count.maxValue, "buffered_count overflow")
        buffered_count := buffered_count + 1
      }
      when(!io_input.fire && io_output.fire) {
        assert(buffered_count =/= 0, "buffered_count underflow")
        buffered_count := buffered_count - 1
      }
      val io_available = buffered_count === 0

      buffered_input.setBlocked()
      io_output.setIdle()

      val rt = Reg(RtCtx())

      val fsm = new StateMachine {
        val init: State = new State with EntryPoint {
          whenIsActive {
            buffered_input.ready := True
            when(buffered_input.valid) {
              val dispatchInfo = buffered_input.payload.lookup[DispatchInfo]
              val issue = buffered_input.payload.lookup[IssuePort[_]]

              val newRt = RtCtx()
              newRt.op := buffered_input.payload.lookup[DividerOperation]
              newRt.aNeg := issue.srcRegData(0)(31)
              newRt.bNeg := issue.srcRegData(1)(31)
              newRt.dividend := issue.srcRegData(0)
              newRt.divisor := issue.srcRegData(1)
              newRt.quotient := 0
              newRt.remainder := 0
              newRt.counter := 0
              newRt.token := dispatchInfo.lookup[CommitToken]
              Machine.report(Seq("begin division: ", issue.srcRegData(0), " ", issue.srcRegData(1)))
              rt := newRt
              goto(work)
            }
          }
        }

        val work: State = new State {
          whenIsActive {
            val preRemainder = rt.remainder(
              spec.dataWidth.value - 2 downto 0
            ) ## rt.dividend(spec.dataWidth.value - 1)
            val dividend =
              rt.dividend(spec.dataWidth.value - 2 downto 0) ## B(0, 1 bits)
            val remainderOverflow = preRemainder.asUInt >= rt.divisor.asUInt
            val remainder = remainderOverflow.mux(
              True -> (preRemainder.asUInt - rt.divisor.asUInt).asBits,
              False -> preRemainder
            )
            val quotient = remainderOverflow.mux(
              True -> (rt
                .quotient(spec.dataWidth.value - 2 downto 0) ## B(1, 1 bits)),
              False -> (rt
                .quotient(spec.dataWidth.value - 2 downto 0) ## B(0, 1 bits))
            )
            rt.dividend := dividend
            rt.quotient := quotient
            rt.remainder := remainder
            rt.counter := rt.counter + 1

            when(rt.counter === rt.counter.maxValue) {
              goto(complete)
            }
          }
        }
        val complete: State = new State {
          whenIsActive {
            val quotient = rt.quotient.asUInt
              .twoComplement(rt.op.signed & (rt.aNeg ^ rt.bNeg))
              .asBits
            val remainder =
              rt.remainder.asUInt.twoComplement(rt.op.signed & rt.aNeg).asBits
            val out = rt.op.useRemainder
              .mux(
                True -> remainder,
                False -> quotient
              )
              .resize(spec.dataWidth)
            io_output.valid := True
            io_output.payload.token := rt.token
            io_output.payload.exception := False
            io_output.payload.regWriteValue(0) := out
            when(io_output.ready) {
              Machine.report(Seq("end division: ", out))
              goto(init)
            }
          }
        }
      }

    }
  }
}
