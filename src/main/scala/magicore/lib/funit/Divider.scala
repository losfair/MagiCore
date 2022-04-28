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
  val div32 = Bool()
}

final class DividerImpl(
    hardType: HardType[PolymorphicDataChain],
    dataWidth: Int,
    enableException: Boolean
) extends FunctionUnitInstance {

  case class RtCtx() extends Bundle {
    val op = DividerOperation()
    val aNeg = Bool()
    val bNeg = Bool()
    val dividend = Bits(dataWidth bits)
    val divisor = Bits(dataWidth bits)
    val quotient = Bits(dataWidth bits)
    val remainder = Bits(dataWidth bits)
    val counter = UInt(log2Up(dataWidth) bits)
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
          newRt.aNeg := issue.srcRegData(0)(dataWidth - 1)
          newRt.bNeg := issue.srcRegData(1)(dataWidth - 1)
          newRt.dividend := issue.srcRegData(0).resized
          newRt.divisor := issue.srcRegData(1).resized
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
              ) << (dataWidth - 1)) && newRt.divisor === 1
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
          dataWidth - 2 downto 0
        ) ## rt.dividend(dataWidth - 1)
        val dividend =
          rt.dividend(dataWidth - 2 downto 0) ## B(0, 1 bits)
        val remainderOverflow = preRemainder.asUInt >= rt.divisor.asUInt
        val remainder = remainderOverflow.mux(
          True -> (preRemainder.asUInt - rt.divisor.asUInt).asBits,
          False -> preRemainder
        )
        val quotient = remainderOverflow.mux(
          True -> (rt
            .quotient(dataWidth - 2 downto 0) ## B(1, 1 bits)),
          False -> (rt
            .quotient(dataWidth - 2 downto 0) ## B(0, 1 bits))
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
          .resize(dataWidth)
        outStream.valid := True
        outStream.payload.token := rt.token
        outStream.payload.exception := MachineException.idle
        outStream.payload.regWriteValue(0) := out.asSInt
          .resize(outStream.payload.regWriteValue(0).getWidth)
          .asBits
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

final class Divider(
    staticTagData: => Data,
    div32: Boolean = false,
    enableException: Boolean = false
) extends FunctionUnit {
  def staticTag: Data = staticTagData
  override def warnOnBlockedIssue = true
  override def generate(
      hardType: HardType[_ <: PolymorphicDataChain]
  ): FunctionUnitInstance = {
    val mspec = Machine.get[MachineSpec]
    val hardTypeGeneric = hardType.asInstanceOf[HardType[PolymorphicDataChain]]
    if (!div32)
      return new DividerImpl(
        hardTypeGeneric,
        mspec.dataWidth.value,
        enableException
      )
    new FunctionUnitInstance {
      val divNativeUnit =
        new DividerImpl(hardTypeGeneric, mspec.dataWidth.value, enableException)
      val div32Unit = new DividerImpl(hardTypeGeneric, 32, enableException)

      val io_input = Stream(hardTypeGeneric())
      val io_output = Stream(CommitRequest(null))
      val io_available =
        divNativeUnit.io_available && div32Unit.io_available

      val demux = StreamDemux(
        io_input,
        io_input.payload.lookup[DividerOperation].div32.asUInt,
        2
      )
      demux(0) >> divNativeUnit.io_input
      demux(1) >> div32Unit.io_input
      StreamArbiterFactory.lowerFirst
        .on(Seq(divNativeUnit.io_output, div32Unit.io_output))
        .stage() >> io_output
    }
  }
}
