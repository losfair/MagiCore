package magicore.lib.funit

import spinal.core._
import spinal.lib._
import magicore.util.PolymorphicDataChain
import magicore.control._
import spinal.lib.fsm._

case class DividerOperation() extends Bundle with PolymorphicDataChain {
  def parentObjects = Seq()

  val useRemainder = Bool()
  val signed = Bool()
}

final class Divider(staticTagData: => Data, enableException: Boolean = false)
    extends FunctionUnit {
  def staticTag: Data = staticTagData
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

      val outStream = Stream(CommitRequest(null))
      outStream >/-> io_output

      val buffered_input = io_input.s2mPipe()

      val buffered_count = Reg(UInt(3 bits)) init (0)
      when(io_input.fire && !outStream.fire) {
        assert(
          buffered_count =/= buffered_count.maxValue,
          "buffered_count overflow"
        )
        buffered_count := buffered_count + 1
      }
      when(!io_input.fire && outStream.fire) {
        assert(buffered_count =/= 0, "buffered_count underflow")
        buffered_count := buffered_count - 1
      }
      val io_available = buffered_count === 0

      buffered_input.setBlocked()
      outStream.setIdle()

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
              newRt.aNeg := issue.srcRegData(0)(spec.dataWidth.value - 1)
              newRt.bNeg := issue.srcRegData(1)(spec.dataWidth.value - 1)
              newRt.dividend := issue.srcRegData(0)
              newRt.divisor := issue.srcRegData(1)
              newRt.quotient := 0
              newRt.remainder := 0
              newRt.counter := 0
              newRt.token := dispatchInfo.lookup[CommitToken]
              rt := newRt

              val hasExc = Bool()

              if (enableException) {
                val signedOverflow =
                  newRt.op.signed && newRt.dividend === (BigInt(
                    1
                  ) << (spec.dataWidth.value - 1)) && newRt.divisor === 1
                val divideByZero = newRt.divisor === 0
                hasExc := signedOverflow || divideByZero
                when(hasExc) {
                  Machine.report(
                    Seq(
                      "division error: signedOverflow=",
                      signedOverflow,
                      " divideByZero=",
                      divideByZero
                    )
                  )
                  goto(divError)
                }
              } else {
                hasExc := False
              }

              when(!hasExc) {
                Machine.report(
                  Seq(
                    "begin division: ",
                    issue.srcRegData(0),
                    " ",
                    issue.srcRegData(1)
                  )
                )
                goto(work)
              }
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
            outStream.valid := True
            outStream.payload.token := rt.token
            outStream.payload.exception := MachineException.idle
            outStream.payload.regWriteValue(0) := out
            when(outStream.ready) {
              Machine.report(
                Seq(
                  "end division: ",
                  out,
                  " useRemainder=",
                  rt.op.useRemainder,
                  " signed=",
                  rt.op.signed
                )
              )
              goto(init)
            }
          }
        }
        val divError: State = if (enableException) new State {
          whenIsActive {
            outStream.valid := True
            outStream.payload.token := rt.token
            outStream.payload.exception.valid := True
            outStream.payload.exception.code := MachineExceptionCode.DIVIDE_ERROR
            when(outStream.ready) {
              Machine.report(Seq("end division error"))
              goto(init)
            }
          }
        }
        else null
      }

    }
  }
}
