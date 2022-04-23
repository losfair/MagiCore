package miniooo.isa.riscv

import spinal.core._
import spinal.lib._
import miniooo.util._
import MiniOoOExt._
import miniooo.control._
import miniooo.frontend._
import scala.reflect._
import miniooo.lib.funit.AluOperation
import miniooo.lib.funit.LsuOperation
import miniooo.lib.funit.AluBranchCondition
import miniooo.lib.funit.AluOpcode
import javax.swing.plaf.multi.MultiOptionPaneUI
import miniooo.lib.funit.DividerOperation
import miniooo.lib.funit.LsuOperationSize

object ImmType extends SpinalEnum(binarySequential) {
  val X, I, H, S, B, J, U =
    newElement()

  def interpret(me: SpinalEnumCraft[ImmType.type], insn: Bits): (Bool, Bits) = {
    val E = RvEncoding
    val imm = E.IMM(insn)
    val out = Bits(32 bits)
    val ok = True
    switch(me) {
      is(I) { out := imm.i_sext }
      is(H) { out := imm.h_sext }
      is(S) { out := imm.s_sext }
      is(B) { out := imm.b_sext }
      is(J) { out := imm.j_sext }
      is(U) { out := imm.u }
      default {
        out.assignDontCare()
        ok := False
      }
    }
    (ok, out)
  }
}

case class DecodePacket() extends Bundle with PolymorphicDataChain {
  private val mspec = Machine.get[MachineSpec]
  private val fspec = Machine.get[FrontendSpec]
  val fetch = FetchPacket()

  val rs1Valid = Bool()
  val rs2Valid = Bool()
  val rdValid = Bool()

  val fuTag = mspec.functionUnitTagType()
  val immType = ImmType()

  def parentObjects = Seq(fetch)

  override def decodeAs[T <: AnyRef](ctag: ClassTag[T]): Option[T] = {
    val E = RvEncoding

    if (ctag == classTag[EarlyException]) {
      val x = EarlyException()
      x.code := fetch.cacheMiss ? EarlyExceptionCode.CACHE_MISS | EarlyExceptionCode.DECODE_ERROR
      Some(x.asInstanceOf[T])
    } else if (ctag == classTag[DecodeInfo]) {
      val x = DecodeInfo(null)
      x.archSrcRegs(0).valid := rs1Valid
      x.archSrcRegs(0).index := fetch.insn(E.rs1Range).asUInt
      x.archSrcRegs(1).valid := rs2Valid
      x.archSrcRegs(1).index := fetch.insn(E.rs2Range).asUInt
      x.archDstRegs(0).valid := rdValid
      x.archDstRegs(0).index := fetch.insn(E.rdRange).asUInt
      x.functionUnitTag := fuTag
      Some(x.asInstanceOf[T])
    } else if (ctag == classTag[AluOperation]) {
      val x = AluOperation()
      x.alu32 := False
      x.predicated := False
      x.setPredicateInsteadOfBranch := False
      x.brCond := fetch
        .insn(6 downto 5)
        .andR
        .mux(
          True -> fetch
            .insn(14 downto 12)
            .mux(
              M"000" -> AluBranchCondition.EQ.craft(),
              M"001" -> AluBranchCondition.NE.craft(),
              M"100" -> AluBranchCondition.LT.craft(),
              M"101" -> AluBranchCondition.GE.craft(),
              M"110" -> AluBranchCondition.LTU.craft(),
              M"111" -> AluBranchCondition.GEU.craft(),
              default -> AluBranchCondition.EQ.craft()
            ), // Branch
          False -> (fetch.insn(
            12
          ) ? AluBranchCondition.LTU | AluBranchCondition.LT) // SLT/SLTU
        )

      val (useConst, const) = ImmType.interpret(immType, fetch.insn)
      x.useConst := useConst
      x.const := const

      val preOpcode =
        fetch.insn(14 downto 12) ## fetch.insn(
          6 downto 2
        )
      x.opcode := preOpcode.mux(
        M"00001100" -> useConst.mux(
          True -> AluOpcode.ADD.craft(),
          False -> fetch
            .insn(30)
            .mux(
              True -> AluOpcode.SUB.craft(),
              False -> AluOpcode.ADD.craft()
            )
        ),
        M"00101100" -> AluOpcode.SLL.craft(),
        M"01-01100" -> AluOpcode.CMP.craft(),
        M"10001100" -> AluOpcode.XOR.craft(),
        M"10101100" -> fetch
          .insn(30)
          .mux(
            True -> AluOpcode.SRA.craft(),
            False -> AluOpcode.SRL.craft()
          ),
        M"11001100" -> AluOpcode.OR.craft(),
        M"11101100" -> AluOpcode.AND.craft(),
        M"---11000" -> AluOpcode.BRANCH.craft(),
        M"---01101" -> AluOpcode.MOV.craft(), // lui
        M"---00101" -> AluOpcode.ADD_TO_PC.craft(), // auipc
        M"---11011" -> AluOpcode.LINK.craft(), // jal
        M"---11001" -> AluOpcode.DYN_BRANCH.craft(), // jalr
        default -> AluOpcode.ADD.craft()
      )

      Some(x.asInstanceOf[T])
    } else if (ctag == classTag[LsuOperation]) {
      val x = LsuOperation()
      x.isStore := fetch.insn(5)
      x.isFence := False
      x.offset := ImmType.interpret(immType, fetch.insn)._2.asSInt
      x.size := fetch
        .insn(13 downto 12)
        .mux(
          M"00" -> LsuOperationSize.BYTE.craft(),
          M"01" -> LsuOperationSize.HALF.craft(),
          default -> LsuOperationSize.WORD.craft()
        )
      Some(x.asInstanceOf[T])
    } else if (ctag == classTag[DividerOperation]) {
      val x = DividerOperation()
      x.signed := False
      x.useRemainder := False
      Some(x.asInstanceOf[T])
    } else {
      None
    }
  }
}

case class RiscvDecoder(
    aluPort: Data,
    earlyExceptionPort: Data,
    lsuPort: Data,
    mulPort: Data,
    divPort: Data
) extends Area {
  val E = RvEncoding
  // TODO: RVC
  private val fspec = Machine.get[FrontendSpec]
  assert(fspec.insnWidth.value == 32)

  val io = new Bundle {
    val input = Stream(FetchPacket())
    val output = Stream(DecodePacket())
    val branchInfoFeedback = BranchInfoFeedback()
  }

  val exc = Machine.get[MachineException]

  val insn = io.input.payload.insn
  val imm = E.IMM(insn)
  val opc = insn(6 downto 0)

  val out = DecodePacket()
  out.fetch := io.input.payload
  out.rs1Valid := False
  out.rs2Valid := False
  out.rdValid := False

  val outPatched = DecodePacket()
  outPatched.rs1Valid := out.rs1Valid && insn(E.rs1Range).asUInt =/= 0
  outPatched.rs2Valid := out.rs2Valid && insn(E.rs2Range).asUInt =/= 0
  outPatched.rdValid := out.rdValid && insn(E.rdRange).asUInt =/= 0
  outPatched.assignUnassignedByName(out)

  when(out.fetch.cacheMiss) {
    outPatched.fuTag := earlyExceptionPort
  }

  io.output << io.input.translateWith(outPatched)

  io.branchInfoFeedback := BranchInfoFeedback.idle

  val brFeedback_offset = UInt(32 bits) assignDontCare ()
  io.branchInfoFeedback.target := io.input.payload.pc + brFeedback_offset

  switch(insn) {
    is(
      E.BEQ(false),
      E.BNE(false),
      E.BLT(false),
      E.BGE(false),
      E.BLTU(false),
      E.BGEU(false)
    ) {
      io.branchInfoFeedback.isConditionalBranch := True
      io.branchInfoFeedback.backward := insn(31)
      brFeedback_offset := imm.b_sext.asUInt

      out.rs1Valid := True
      out.rs2Valid := True
      out.fuTag := aluPort
      out.immType := ImmType.B
    }
    is(E.JAL(false)) {
      io.branchInfoFeedback.isUnconditionalStaticBranch := True
      brFeedback_offset := imm.j_sext.asUInt
      out.rdValid := True
      out.fuTag := aluPort
      out.immType := ImmType.J
    }
    is(E.JALR) {
      io.branchInfoFeedback.isUnconditionalDynamicBranch := True
      out.rdValid := True
      out.rs1Valid := True
      out.fuTag := aluPort
      out.immType := ImmType.I
    }
    is(E.ADD, E.SUB, E.SLL, E.SLT, E.SLTU, E.XOR, E.SRL, E.SRA, E.OR, E.AND) {
      out.rdValid := True
      out.rs1Valid := True
      out.rs2Valid := True
      out.fuTag := aluPort
      out.immType := ImmType.X
    }
    is(E.ADDI, E.SLTI, E.SLTIU, E.XORI, E.ORI, E.ANDI, E.SLLI, E.SRLI, E.SRAI) {
      out.rdValid := True
      out.rs1Valid := True
      out.fuTag := aluPort
      out.immType := ImmType.I
    }
    is(E.LUI, E.AUIPC) {
      out.rdValid := True
      out.fuTag := aluPort
      out.immType := ImmType.U
    }
    is(E.LB, E.LH, E.LW, E.LBU, E.LHU) {
      out.rdValid := True
      out.rs1Valid := True
      out.fuTag := lsuPort
      out.immType := ImmType.I
    }
    is(E.SB, E.SH, E.SW) {
      out.rs1Valid := True
      out.rs2Valid := True
      out.fuTag := lsuPort
      out.immType := ImmType.S
    }
    is(E.MUL, E.MULH, E.MULHSU, E.MULHU) {
      out.rdValid := True
      out.rs1Valid := True
      out.rs2Valid := True
      out.fuTag := mulPort
      out.immType := ImmType.X
    }
    is(E.DIV, E.DIVU, E.REM, E.REMU) {
      out.rdValid := True
      out.rs1Valid := True
      out.rs2Valid := True
      out.fuTag := divPort
      out.immType := ImmType.X
    }
    default {
      out.fuTag := earlyExceptionPort
      out.immType := ImmType.X
    }
  }

}
