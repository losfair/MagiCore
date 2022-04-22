package miniooo.lib.funit

import spinal.core._
import spinal.lib._
import miniooo.util.PolymorphicDataChain
import miniooo.control._

case class AluConfig(
    alu32: Boolean = false,
    linkOffset: Int = 4
)

case class AluBranchContext(
    globalHistoryWidth: BitCount
) extends Bundle
    with PolymorphicDataChain {
  private val spec = Machine.get[MachineSpec]
  def parentObjects = Seq()

  val pc = spec.dataType
  val predictedBranchValid = Bool()
  val predictedBranchTarget = spec.dataType
}

object AluOpcode extends SpinalEnum(binarySequential) {
  val ADD, SUB, AND, OR, XOR, MOV, SLL, SRL, SRA, CMP, ADD_TO_PC,
      LOAD_PREDICATE_BUFFER, BRANCH, DYN_BRANCH, LINK =
    newElement()
}

object AluBranchCondition extends SpinalEnum(binarySequential) {
  val LT, LTU, LE, GT, GE, GEU, EQ, NE = newElement()
}

case class AluOperation() extends Bundle with PolymorphicDataChain {
  private val spec = Machine.get[MachineSpec]

  def parentObjects = Seq()

  val opcode = AluOpcode()
  val predicated = Bool()
  val alu32 = Bool()
  val const = Machine.get[MachineSpec].dataType
  val useConst = Bool()

  // Branch control
  val setPredicateInsteadOfBranch = Bool()
  val brCond = AluBranchCondition()

  def fillBranchFieldsForNonBranch() {
    setPredicateInsteadOfBranch.assignDontCare()
    brCond.assignDontCare()
  }
}

class Alu(staticTagData: => Data, c: AluConfig) extends FunctionUnit {
  private val spec = Machine.get[MachineSpec]

  def staticTag: Data = staticTagData
  override def isAlu: Boolean = true
  override def generate(
      hardType: HardType[_ <: PolymorphicDataChain]
  ): FunctionUnitInstance = {
    object BypassInfo {
      def idle: BypassInfo = {
        val x = BypassInfo()
        x.assignDontCare()
        x.valid := False
        x
      }
    }

    case class BypassInfo() extends Bundle {
      val valid = Bool()
      val index = spec.physRegIndexType
      val data = spec.dataType
    }

    new FunctionUnitInstance {
      val io_available = True
      val io_input = Stream(hardType())
      val io_output = Stream(CommitRequest(null))

      val currentPredicate = Reg(Bool()) init (false)
      val predicateBuffer = Reg(spec.dataType) init (0)

      val in = io_input.payload
      val out = CommitRequest(null)

      val op = in.lookup[AluOperation]
      val decode = in.lookup[DecodeInfo]
      val rename = in.lookup[RenameInfo]
      val dispatchInfo = in.lookup[DispatchInfo]
      val issue = in.lookup[IssuePort[_]]

      val srcRegBypass = Machine.get[MachineException].resetArea {
        Reg(BypassInfo()) init (BypassInfo.idle)
      }

      val srcRegValues = (0 until spec.maxNumSrcRegsPerInsn).map(i => {
        val matches = decode
          .archSrcRegs(i)
          .valid && srcRegBypass.valid && srcRegBypass.index === rename
          .physSrcRegs(i)
        matches
          .mux(
            True -> srcRegBypass.data,
            False -> issue.srcRegData(i)
          )
          .asUInt
      })

      val a = srcRegValues(0)
      val b = op.useConst ? op.const.asUInt | srcRegValues(1)
      out.token := dispatchInfo.lookup[CommitToken]
      out.exception := MachineException.idle

      val condLt = srcRegValues(0).asSInt < srcRegValues(1).asSInt
      val condLtu = srcRegValues(0) < srcRegValues(1)
      val condEq = srcRegValues(0) === srcRegValues(1)
      val condLe = condLt || condEq
      val condGt = !condLe
      val condGe = !condLt
      val condGeu = !condLtu
      val condNe = !condEq
      val cond = op.brCond.mux(
        AluBranchCondition.LT -> condLt,
        AluBranchCondition.LTU -> condLtu,
        AluBranchCondition.LE -> condLe,
        AluBranchCondition.GT -> condGt,
        AluBranchCondition.GE -> condGe,
        AluBranchCondition.GEU -> condGeu,
        AluBranchCondition.EQ -> condEq,
        AluBranchCondition.NE -> condNe
      )

      val brCtx = in.tryLookup[AluBranchContext]

      val outValue = UInt(out.regWriteValue(0).getWidth bits)
      outValue.assignDontCare()

      val linkValue =
        if (brCtx.isDefined) brCtx.get.pc.asUInt + c.linkOffset else null
      val pcAddConst =
        if (brCtx.isDefined)
          (brCtx.get.pc.asSInt + op.const.asSInt.resized).asUInt
        else null

      switch(op.opcode) {
        is(AluOpcode.ADD) {
          outValue := a + b
        }
        is(AluOpcode.SUB) {
          outValue := a - b
        }
        is(AluOpcode.AND) {
          outValue := a & b
        }
        is(AluOpcode.OR) {
          outValue := a | b
        }
        is(AluOpcode.XOR) {
          outValue := a ^ b
        }
        is(AluOpcode.MOV) {
          outValue := b
        }
        is(AluOpcode.SLL) {
          outValue := (a << b(
            log2Up(spec.dataWidth.value) - 1 downto 0
          )).resized
        }
        is(AluOpcode.SRL) {
          outValue := a >> b(log2Up(spec.dataWidth.value) - 1 downto 0)
        }
        is(AluOpcode.SRA) {
          outValue := (a.asSInt >> b(
            log2Up(spec.dataWidth.value) - 1 downto 0
          )).asUInt
        }
        is(AluOpcode.CMP) {
          outValue := cond.asUInt.resized
        }
        is(AluOpcode.LOAD_PREDICATE_BUFFER) {
          predicateBuffer := a.asBits
        }
        if (brCtx.isDefined) {
          is(AluOpcode.ADD_TO_PC) {
            outValue := pcAddConst
          }
          is(AluOpcode.LINK) {
            when(io_input.valid) {
              assert(
                brCtx.get.predictedBranchValid,
                "LINK operation must be used with a valid branch"
              )
            }
            outValue := linkValue
          }
          is(AluOpcode.BRANCH) {
            import AluBranchCondition._
            when(io_input.fire) {
              Machine.report(
                Seq("branch operands: ", srcRegValues(0), " ", srcRegValues(1))
              )
            }
            when(op.setPredicateInsteadOfBranch) {
              currentPredicate := !cond
            } otherwise {
              val computedTarget = pcAddConst.asBits

              // If the branch decisions are different, or the target addresses are different
              when(io_input.valid) {
                assert(
                  !brCtx.get.predictedBranchValid || brCtx.get.predictedBranchTarget === computedTarget,
                  Seq(
                    "branch prediction/computation mismatch: predicted=",
                    brCtx.get.predictedBranchTarget,
                    " computed=",
                    computedTarget
                  )
                )
              }
              when(
                cond =/= brCtx.get.predictedBranchValid
              ) {
                out.exception.valid := True
                out.exception.code := MachineExceptionCode.BRANCH_MISS
                out.exception.brDstAddr := computedTarget
                out.exception.brIsConst := True
                out.exception.brTaken := cond
              }
            }
          }
          is(AluOpcode.DYN_BRANCH) {
            when(io_input.valid) {
              assert(op.useConst, "DYN_BRANCH must use a constant")
            }
            val target = (a + op.const.asUInt.resized).asBits
            outValue := linkValue
            when(
              !brCtx.get.predictedBranchValid || target =/= brCtx.get.predictedBranchTarget
            ) {
              out.exception.valid := True
              out.exception.code := MachineExceptionCode.BRANCH_MISS
              out.exception.brDstAddr := target
              out.exception.brIsConst := False
              out.exception.brTaken := True
            }
          }
        }
      }

      if (c.alu32) {
        when(op.alu32) {
          out.regWriteValue(0) := outValue(31 downto 0).asBits.resized
        } otherwise {
          out.regWriteValue(0) := outValue.asBits
        }
      } else {
        out.regWriteValue(0) := outValue.asBits
      }

      // `valid := False` write and `prf.write` happen on the same cycle.
      when(io_output.fire) {
        srcRegBypass.valid := False
      }

      when(io_input.fire) {
        when(op.predicated && !currentPredicate) {
          out.regWriteValue(0) := predicateBuffer
        }
        when(decode.archDstRegs(0).valid) {
          srcRegBypass.valid := True
          srcRegBypass.index := rename.physDstRegs(0)
          srcRegBypass.data := out.regWriteValue(0)
        }
      }

      io_output <-< io_input.translateWith(out)
    }
  }

}
