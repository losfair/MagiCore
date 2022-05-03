package magicore.isa.riscv

import spinal.core._
import spinal.lib._
import magicore.util._
import MagiCoreExt._
import magicore.control._
import magicore.frontend._
import scala.reflect._
import magicore.lib.funit.AluOperation
import magicore.lib.funit.LsuOperation
import magicore.lib.funit.AluBranchCondition
import magicore.lib.funit.AluOpcode
import javax.swing.plaf.multi.MultiOptionPaneUI
import magicore.lib.funit.DividerOperation
import magicore.lib.funit.LsuOperationSize
import magicore.lib.funit.EarlyException
import magicore.lib.funit.MultiplierOperation
import magicore.lib.funit.SlowAluOperation
import magicore.lib.funit.SlowAluOpcode
import spinal.lib.fsm._
import scala.collection.mutable.ArrayBuffer

object ImmType extends SpinalEnum(binarySequential) {
  private val mspec = Machine.get[MachineSpec]

  val X, I, H, S, B, J, U =
    newElement()

  def interpret(me: SpinalEnumCraft[ImmType.type], insn: Bits): (Bool, Bits) = {
    val E = RvEncoding
    val imm = E.IMM(insn)
    val out = Bits(32 bits)
    val replaceOperandBwithConst = False
    switch(me) {
      is(I) {
        out := imm.i_sext
        replaceOperandBwithConst := True
      }
      is(H) { out := imm.h_sext }
      is(S) { out := imm.s_sext }
      is(B) { out := imm.b_sext }
      is(J) { out := imm.j_sext }
      is(U) {
        out := imm.u
        replaceOperandBwithConst := True
      }
      default {
        out.assignDontCare()
      }
    }
    (replaceOperandBwithConst, out.asSInt.resize(mspec.dataWidth).asBits)
  }
}

case class DecodePacket() extends Bundle with PolymorphicDataChain {
  private val mspec = Machine.get[MachineSpec]
  private val fspec = Machine.get[FrontendSpec]
  val fetch = FetchPacket()

  val isMicroOp = Bool()
  val rs1Valid = Bool()
  val rs2Valid = Bool()
  val rdValid = Bool()

  val fuTag = mspec.functionUnitTagType()
  val earlyExc = EarlyException()
  val immType = ImmType()
  val isStore = Bool()

  def parentObjects = Seq(fetch)

  private def interpretAmoMemop(x: LsuOperation) {
    x.isStore := fetch.insn(27)
    x.isFence := False
    x.isLrSc := True
    x.offset := 0
    x.size := fetch.insn(12) ? LsuOperationSize.DOUBLE | LsuOperationSize.WORD
    x.signExt := True
  }

  override def decodeAs[T <: AnyRef](ctag: ClassTag[T]): Option[T] = {
    val E = RvEncoding

    if (ctag == classTag[EarlyException]) {
      Some(earlyExc.asInstanceOf[T])
    } else if (ctag == classTag[DecodeInfo]) {
      val x = DecodeInfo(null)
      x.archSrcRegs(0).valid := rs1Valid
      x.archSrcRegs(0).waitValue := True
      x.archSrcRegs(0).index := fetch.insn(E.rs1Range).asUInt
      x.archSrcRegs(1).valid := rs2Valid
      x.archSrcRegs(1).waitValue := !isStore
      x.archSrcRegs(1).index := fetch.insn(E.rs2Range).asUInt
      x.archDstRegs(0).valid := rdValid
      x.archDstRegs(0).index := fetch.insn(E.rdRange).asUInt
      x.isMicroOp := isMicroOp
      x.functionUnitTag := fuTag
      Some(x.asInstanceOf[T])
    } else if (ctag == classTag[AluOperation]) {
      val x = AluOperation()

      // `JAL` is an exception that has bit 3 set but should not enable alu32
      x.alu32 := fetch.insn(3) && fetch.insn(6 downto 2) =/= B"11011"

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

      val (replaceOperandBwithConst, const) =
        ImmType.interpret(immType, fetch.insn)
      x.replaceOperandBwithConst := replaceOperandBwithConst
      x.const := const

      val preOpcode =
        fetch.insn(14 downto 12) ## fetch.insn(
          6 downto 2
        )
      x.opcode := preOpcode.mux(
        M"0000-1-0" -> replaceOperandBwithConst.mux(
          True -> AluOpcode.ADD.craft(),
          False -> fetch
            .insn(30)
            .mux(
              True -> AluOpcode.SUB.craft(),
              False -> AluOpcode.ADD.craft()
            )
        ),
        M"0010-1-0" -> AluOpcode.SLL.craft(),
        M"01-0-100" -> AluOpcode.CMP.craft(),
        M"1000-100" -> AluOpcode.XOR.craft(),
        M"1010-1-0" -> fetch
          .insn(30)
          .mux(
            True -> AluOpcode.SRA.craft(),
            False -> AluOpcode.SRL.craft()
          ),
        M"1100-100" -> AluOpcode.OR.craft(),
        M"1110-100" -> AluOpcode.AND.craft(),
        M"---11000" -> AluOpcode.BRANCH.craft(),
        M"---01101" -> AluOpcode.MOV.craft(), // lui
        M"---00101" -> AluOpcode.ADD_TO_PC.craft(), // auipc
        M"---11011" -> AluOpcode.LINK.craft(), // jal
        M"---11001" -> AluOpcode.DYN_BRANCH.craft(), // jalr
        default -> AluOpcode.ADD.craft()
      )

      Some(x.asInstanceOf[T])
    } else if (ctag == classTag[LsuOperation]) {
      val opc = fetch.insn(6 downto 2)
      val x = LsuOperation()
      x.isMicroOp := isMicroOp

      when(opc === B"01011") {
        // AMO
        interpretAmoMemop(x)
      } otherwise {
        x.isStore := fetch.insn(5)
        x.isFence := opc === B"00011"
        x.isLrSc := False
        x.offset := ImmType.interpret(immType, fetch.insn)._2.asSInt
        x.size := fetch
          .insn(13 downto 12)
          .mux(
            B"00" -> LsuOperationSize.BYTE.craft(),
            B"01" -> LsuOperationSize.HALF.craft(),
            B"10" -> LsuOperationSize.WORD.craft(),
            B"11" -> LsuOperationSize.DOUBLE.craft()
          )
        x.signExt := !fetch.insn(14)
      }
      Some(x.asInstanceOf[T])
    } else if (ctag == classTag[DividerOperation]) {
      val x = DividerOperation()
      x.div32 := fetch.insn(3)
      x.signed := !fetch.insn(12)
      x.useRemainder := fetch.insn(13)
      Some(x.asInstanceOf[T])
    } else if (ctag == classTag[MultiplierOperation]) {
      val op = MultiplierOperation(enableMul32 = true)
      op.mul32 := fetch.insn(3)
      switch(fetch.insn(13 downto 12)) {
        is(B"00") {
          // MUL
          op.aSigned := False
          op.bSigned := False
          op.upperHalf := False
        }
        is(B"01") {
          // MULH
          op.aSigned := True
          op.bSigned := True
          op.upperHalf := True
        }
        is(B"10") {
          // MULHSU
          op.aSigned := True
          op.bSigned := False
          op.upperHalf := True
        }
        default {
          // MULHU
          op.aSigned := False
          op.bSigned := False
          op.upperHalf := True
        }
      }
      Some(op.asInstanceOf[T])
    } else if (ctag == classTag[SlowAluOperation]) {
      val op = SlowAluOperation()
      switch(fetch.insn) {
        is(E.CLZ) {
          op.opcode := SlowAluOpcode.CLZ
        }
        is(E.CTZ) {
          op.opcode := SlowAluOpcode.CTZ
        }
        is(E.MAX) {
          op.opcode := SlowAluOpcode.MAX_S
        }
        is(E.MAXU) {
          op.opcode := SlowAluOpcode.MAX_U
        }
        is(E.MIN) {
          op.opcode := SlowAluOpcode.MIN_S
        }
        is(E.MINU) {
          op.opcode := SlowAluOpcode.MIN_U
        }
        default {
          op.assignDontCare()
        }
      }
      Some(op.asInstanceOf[T])
    } else {
      None
    }
  }
}

case class DecodeIntrInjectionInfo() extends Bundle {
  val cause = UInt(4 bits)
}

case class RvAnnotatedFetchPacket() extends Bundle {
  val fetch = FetchPacket()
  val microOp = Bool()
  val unsuppressRs1 = Bool()
  val unsuppressRs2 = Bool()
  val unsuppressRd = Bool()
}

case class RiscvDecoder(
    rv64: Boolean,
    aluPort: Data,
    earlyExceptionPort: Data,
    lsuPort: Data,
    mulPort: Data,
    divPort: Data,
    csrPort: Data,
    slowAluPort: Data
) extends Area {
  import RvDecoderExt._

  val E = RvEncoding
  // TODO: RVC
  private val fspec = Machine.get[FrontendSpec]
  assert(fspec.insnWidth.value == 32)

  private val intrSvc = Machine.get[RvInterruptService]

  val io = new Bundle {
    val input = Stream(FetchPacket())
    val output = Stream(DecodePacket())
    val branchInfoFeedback = BranchInfoFeedback()
  }

  val inputStream = io.input.rvAnnotate.expandMicroOps

  val exc = Machine.get[MachineException]

  val wantStall = False

  val stalled = Reg(
    Bool()
  ) init (false) setWhen (inputStream.fire && wantStall) clearWhen (exc.valid)

  val insn = inputStream.payload.fetch.insn
  val imm = E.IMM(insn)
  val opc = insn(6 downto 0)

  val out = DecodePacket()
  out.fetch := inputStream.payload.fetch
  out.isMicroOp := inputStream.payload.microOp
  out.rs1Valid := False
  out.rs2Valid := False
  out.rdValid := False
  out.earlyExc.assignDontCare()
  out.isStore := False

  val outPatched = DecodePacket()

  // Micro-ops can use R0 as a scratch register
  outPatched.rs1Valid := out.rs1Valid && (insn(
    E.rs1Range
  ).asUInt =/= 0 || inputStream.payload.unsuppressRs1)
  outPatched.rs2Valid := out.rs2Valid && (insn(
    E.rs2Range
  ).asUInt =/= 0 || inputStream.payload.unsuppressRs2)
  outPatched.rdValid := out.rdValid && (insn(
    E.rdRange
  ).asUInt =/= 0 || inputStream.payload.unsuppressRd)
  outPatched.assignUnassignedByName(out)

  when(out.fetch.cacheMiss) {
    outPatched.fuTag := earlyExceptionPort
    outPatched.earlyExc.code := MachineExceptionCode.INSN_CACHE_MISS
    wantStall := True
  }

  // Suppress interrupts during micro-op sequences.
  when(intrSvc.trigger && !inputStream.payload.microOp) {
    outPatched.fuTag := earlyExceptionPort
    outPatched.earlyExc.code := MachineExceptionCode.EXT_INTERRUPT
    wantStall := True
  }

  val discardOutput = False
  io.output << inputStream
    .translateWith(outPatched)
    .continueWhen(!stalled)
    .throwWhen(discardOutput)

  def discardIt() {
    discardOutput := True
    out.fuTag.assignDontCare()
    out.immType.assignDontCare()
  }

  // Fast branch feedback path - do not go through microcode sequencer
  val fastBranchFeedbackLogic = new Area {
    io.branchInfoFeedback := BranchInfoFeedback.idle

    val brFeedback_offset = UInt(32 bits) assignDontCare ()
    switch(io.input.payload.insn) {
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
      }
      is(E.JAL(false)) {
        io.branchInfoFeedback.isUnconditionalStaticBranch := True
        brFeedback_offset := imm.j_sext.asUInt
      }
      is(E.JALR) {
        io.branchInfoFeedback.isUnconditionalDynamicBranch := True
      }
    }
    io.branchInfoFeedback.target := io.input.payload.pc + brFeedback_offset
  }

  val csr = Machine.get[RvCsrFileReg]

  switch(insn) {
    is(
      E.BEQ(false),
      E.BNE(false),
      E.BLT(false),
      E.BGE(false),
      E.BLTU(false),
      E.BGEU(false)
    ) {
      out.rs1Valid := True
      out.rs2Valid := True
      out.fuTag := aluPort
      out.immType := ImmType.B
    }
    is(E.JAL(false)) {
      out.rdValid := True
      out.fuTag := aluPort
      out.immType := ImmType.J
    }
    is(E.JALR) {
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
    if (rv64) {
      is(E.ADDW, E.SUBW, E.SLLW, E.SRLW, E.SRAW) {
        out.rdValid := True
        out.rs1Valid := True
        out.rs2Valid := True
        out.fuTag := aluPort
        out.immType := ImmType.X
      }
    }
    is(E.ADDI, E.SLTI, E.SLTIU, E.XORI, E.ORI, E.ANDI, E.SLLI, E.SRLI, E.SRAI) {
      out.rdValid := True
      out.rs1Valid := True
      out.fuTag := aluPort
      out.immType := ImmType.I
    }
    if (rv64) {
      is(E.ADDIW, E.SLLIW, E.SRLIW, E.SRAIW) {
        out.rdValid := True
        out.rs1Valid := True
        out.fuTag := aluPort
        out.immType := ImmType.I
      }
    }
    is(E.CLZ, E.CTZ) {
      out.rdValid := True
      out.rs1Valid := True
      out.fuTag := slowAluPort
      out.immType := ImmType.X
    }
    is(E.MAX, E.MAXU, E.MIN, E.MINU) {
      out.rdValid := True
      out.rs1Valid := True
      out.rs2Valid := True
      out.fuTag := slowAluPort
      out.immType := ImmType.X
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
    if (rv64) {
      is(E.LD, E.LWU) {
        out.rdValid := True
        out.rs1Valid := True
        out.fuTag := lsuPort
        out.immType := ImmType.I
      }
    }
    is(E.SB, E.SH, E.SW) {
      out.rs1Valid := True
      out.rs2Valid := True
      out.fuTag := lsuPort
      out.immType := ImmType.S
      out.isStore := True
    }
    if (rv64) {
      is(E.SD) {
        out.rs1Valid := True
        out.rs2Valid := True
        out.fuTag := lsuPort
        out.immType := ImmType.S
        out.isStore := True
      }
    }
    is(E.LR_W) {
      out.rdValid := True
      out.rs1Valid := True
      out.fuTag := lsuPort
      out.immType := ImmType.X
    }
    is(E.SC_W) {
      out.rdValid := True
      out.rs1Valid := True
      out.rs2Valid := True
      out.fuTag := lsuPort
      out.immType := ImmType.X
      out.isStore := True
    }
    if (rv64) {
      is(E.LR_D) {
        out.rdValid := True
        out.rs1Valid := True
        out.fuTag := lsuPort
        out.immType := ImmType.X
      }
      is(E.SC_D) {
        out.rdValid := True
        out.rs1Valid := True
        out.rs2Valid := True
        out.fuTag := lsuPort
        out.immType := ImmType.X
        out.isStore := True
      }
    }
    is(E.MUL, E.MULH, E.MULHSU, E.MULHU) {
      out.rdValid := True
      out.rs1Valid := True
      out.rs2Valid := True
      out.fuTag := mulPort
      out.immType := ImmType.X
    }
    if (rv64) {
      is(E.MULW) {
        out.rdValid := True
        out.rs1Valid := True
        out.rs2Valid := True
        out.fuTag := mulPort
        out.immType := ImmType.X
      }
    }
    is(E.DIV, E.DIVU, E.REM, E.REMU) {
      out.rdValid := True
      out.rs1Valid := True
      out.rs2Valid := True
      out.fuTag := divPort
      out.immType := ImmType.X
    }
    if (rv64) {
      is(E.DIVW, E.DIVUW, E.REMW, E.REMUW) {
        out.rdValid := True
        out.rs1Valid := True
        out.rs2Valid := True
        out.fuTag := divPort
        out.immType := ImmType.X
      }
    }
    is(E.FENCE) {
      val p_iorw = insn(27 downto 24)
      val s_iorw = insn(23 downto 20)
      when(p_iorw === B"0001" && s_iorw === B"0001") {
        // fence w, w
        discardIt()
      } elsewhen (p_iorw === B"0001") {
        // fence w, *
        out.fuTag := lsuPort
        out.immType := ImmType.X
      } otherwise {
        out.fuTag := earlyExceptionPort
        out.immType := ImmType.X
        out.earlyExc.code := MachineExceptionCode.SERIALIZE
        wantStall := True
      }
    }
    is(E.CSRRW, E.CSRRS, E.CSRRC) {
      out.rdValid := True
      out.rs1Valid := True
      out.fuTag := csrPort
      out.immType := ImmType.X
    }
    is(E.CSRRWI, E.CSRRSI, E.CSRRCI) {
      out.rdValid := True
      out.fuTag := csrPort
      out.immType := ImmType.X
    }
    is(E.FENCEI) {
      out.fuTag := earlyExceptionPort
      out.immType := ImmType.X
      out.earlyExc.code := MachineExceptionCode.INSN_CACHE_FLUSH
      wantStall := True
    }
    is(E.MRET) {
      out.fuTag := earlyExceptionPort
      out.immType := ImmType.X

      when(csr.csrFile.priv === RvPrivLevel.M) {
        out.earlyExc.code := MachineExceptionCode.EXCEPTION_RETURN
      } otherwise {
        out.earlyExc.code := MachineExceptionCode.DECODE_ERROR
      }

      wantStall := True
    }
    is(E.ECALL) {
      out.fuTag := earlyExceptionPort
      out.immType := ImmType.X
      out.earlyExc.code := MachineExceptionCode.ENV_CALL
      wantStall := True
    }
    is(E.WFI) {
      out.fuTag := earlyExceptionPort
      out.immType := ImmType.X
      out.earlyExc.code := MachineExceptionCode.WFI
      wantStall := True
    }
    default {
      out.fuTag := earlyExceptionPort
      out.immType := ImmType.X
      out.earlyExc.code := MachineExceptionCode.DECODE_ERROR
      wantStall := True
    }
  }

}

object RvDecoderExt {
  implicit class FetchStreamExt(fetch: Stream[FetchPacket]) {
    def rvAnnotate: Stream[RvAnnotatedFetchPacket] = {
      val x = RvAnnotatedFetchPacket()
      x.fetch := fetch.payload
      x.microOp := False
      x.unsuppressRs1 := False
      x.unsuppressRs2 := False
      x.unsuppressRd := False
      fetch.translateWith(x)
    }
  }

  implicit class AnnotatedFetchStreamExt(s: Stream[RvAnnotatedFetchPacket]) {
    def expandMicroOps: Stream[RvAnnotatedFetchPacket] =
      Machine.get[MachineException].resetArea {
        val E = RvEncoding
        val out =
          Stream(RvAnnotatedFetchPacket()) setCompositeName (s, postfix =
            "expandMicroOps")
        out.setIdle()

        def genAmoState(d: Bool, template: Bits): StateMachine = {
          val internalScratchCsr0 = 0xbc0 // internal.scratch0
          new StateMachine {
            val init = new State with EntryPoint {
              whenIsActive {
                val data = RvAnnotatedFetchPacket()
                data.fetch.insn := E.LR_W.value
                data.fetch.insn(E.rdRange) := 0
                data.fetch.insn(E.rs1Range) := s.payload.fetch.insn(E.rs1Range)
                data.fetch.insn(26 downto 25) := 0 // aq/rl
                data.fetch.insn(12) := d
                data.fetch.assignUnassignedByName(s.payload.fetch)
                data.microOp := False // Do not suppress interrupts here
                data.unsuppressRd := True
                data.unsuppressRs1 := False
                data.unsuppressRs2 := False
                out.valid := True
                out.payload := data
                when(out.ready) {
                  goto(buffer)
                }
              }
            }
            val buffer = new State {
              whenIsActive {
                val data = RvAnnotatedFetchPacket()
                data.fetch.insn := E.CSRRW.value
                data.fetch.insn(31 downto 20) := internalScratchCsr0
                data.fetch.insn(E.rdRange) := 0
                data.fetch.insn(E.rs1Range) := 0
                data.fetch.insn(E.rs2Range) := 0
                data.fetch.assignUnassignedByName(s.payload.fetch)
                data.microOp := True
                data.unsuppressRd := False
                data.unsuppressRs1 := True
                data.unsuppressRs2 := False
                out.valid := True
                out.payload := data
                when(out.ready) {
                  goto(compute)
                }
              }
            }
            val compute = new State {
              whenIsActive {
                val data = RvAnnotatedFetchPacket()
                val insn = Bits(32 bits)
                insn := template
                insn(E.rdRange) := 0
                insn(E.rs1Range) := 0
                insn(E.rs2Range) := s.payload.fetch.insn(E.rs2Range)
                data.fetch.insn := insn
                data.fetch.assignUnassignedByName(s.payload.fetch)
                data.microOp := True
                data.unsuppressRd := True
                data.unsuppressRs1 := True
                data.unsuppressRs2 := False
                out.valid := True
                out.payload := data
                when(out.ready) {
                  goto(sc)
                }
              }
            }
            val sc = new State {
              whenIsActive {
                val data = RvAnnotatedFetchPacket()
                data.fetch.insn := E.SC_W.value
                data.fetch.insn(E.rdRange) := 0
                data.fetch.insn(E.rs1Range) := s.payload.fetch.insn(E.rs1Range)
                data.fetch.insn(E.rs2Range) := 0
                data.fetch.insn(26 downto 25) := 0 // aq/rl
                data.fetch.insn(12) := d
                data.fetch.assignUnassignedByName(s.payload.fetch)
                data.microOp := True
                data.unsuppressRd := False
                data.unsuppressRs1 := False
                data.unsuppressRs2 := True
                out.valid := True
                out.payload := data
                when(out.ready) {
                  goto(result)
                }
              }
            }
            val result = new State {
              whenIsActive {
                val data = RvAnnotatedFetchPacket()
                data.fetch.insn := E.CSRRS.value
                data.fetch.insn(31 downto 20) := internalScratchCsr0
                data.fetch.insn(E.rdRange) := s.payload.fetch.insn(E.rdRange)
                data.fetch.insn(E.rs1Range) := 0
                data.fetch.insn(E.rs2Range) := 0
                data.fetch.assignUnassignedByName(s.payload.fetch)
                data.microOp := True
                data.unsuppressRd := False
                data.unsuppressRs1 := False
                data.unsuppressRs2 := False
                out.valid := True
                out.payload := data
                s.ready := out.ready
                when(out.ready) {
                  exit()
                }
              }
            }
          }
        }

        val d = Reg(Bool())
        val template = Reg(Bits(32 bits))

        s.setBlocked()

        val fsm = new StateMachine {
          val init: State = new State with EntryPoint {
            whenIsActive {
              def jump(
                  matches: MaskedLiteral,
                  newD: Bool,
                  newTemplate: BigInt
              ) {
                is(matches) {
                  d := newD
                  template := newTemplate
                  goto(emitAmo)
                }
              }

              when(s.valid) {
                switch(s.payload.fetch.insn) {
                  jump(E.AMOADD_D, True, E.ADD.value)
                  jump(E.AMOADD_W, False, E.ADD.value)
                  jump(E.AMOXOR_D, True, E.XOR.value)
                  jump(E.AMOXOR_W, False, E.XOR.value)
                  jump(E.AMOAND_D, True, E.AND.value)
                  jump(E.AMOAND_W, False, E.AND.value)
                  jump(E.AMOOR_D, True, E.OR.value)
                  jump(E.AMOOR_W, False, E.OR.value)
                  jump(E.AMOMIN_D, True, E.MIN.value)
                  jump(E.AMOMIN_W, False, E.MIN.value)
                  jump(E.AMOMAX_D, True, E.MAX.value)
                  jump(E.AMOMAX_W, False, E.MAX.value)
                  jump(E.AMOMINU_D, True, E.MINU.value)
                  jump(E.AMOMINU_W, False, E.MINU.value)
                  jump(E.AMOMAXU_D, True, E.MAXU.value)
                  jump(E.AMOMAXU_W, False, E.MAXU.value)
                  default {
                    out << s
                  }
                }
              }
            }
          }
          val emitAmo: State =
            new StateFsm(genAmoState(d = d, template = template)) {
              whenCompleted {
                goto(init)
              }
            }
        }

        out
      }

  }
}

/*

class RvSequencer extends Area {
  case class Token(offset: Int, len: Int)
  case class Entry() extends Bundle {
    val template = Bits(32 bits)
  }
  val maxCapacity = 128
  val indexWidth = log2Up(maxCapacity) bits
  val rom = Mem(Bits(32 bits), maxCapacity)

  val sequences: ArrayBuffer[Bits] = new ArrayBuffer()

  val currentIndex = Reg(UInt(indexWidth))
  val remaining = Reg(UInt(indexWidth)) init (0)

  val output = Stream(Bits(32 bits))
  output.valid := remaining =/= 0
  output.payload := rom(currentIndex)

  when(output.fire) {
    remaining := remaining - 1
    currentIndex := currentIndex + 1
  }

  Component.current.afterElaboration {
    rom.init(sequences.padTo(maxCapacity, B(0, 32 bits)))
  }

  def add(seq: Seq[Bits]): Token = {
    val offset = sequences.length
    sequences ++= seq
    Token(offset = offset, len = seq.size)
  }

  def addBigInt(seq: Seq[BigInt]): Token = {
    val offset = sequences.length
    sequences ++= seq.map(x => B(x, 32 bits))
    Token(offset = offset, len = seq.size)
  }

  def trigger(token: Token): Unit = {
    currentIndex := token.offset
    remaining := token.len
  }
}
 */
