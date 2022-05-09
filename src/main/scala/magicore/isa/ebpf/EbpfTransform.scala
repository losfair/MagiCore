package magicore.isa.ebpf

import spinal.core._
import spinal.lib._
import magicore.util._
import MagiCoreExt._
import magicore.control._
import magicore.frontend._
import magicore.lib.funit._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.misc.SizeMapping
import magicore.isa.riscv.RvEncoding

case class EbpfTransform() extends Area {
  case class State() extends Bundle {
    val skipIt = Bool()
    val immRs2ExpandStage1 = Bool()
    val immRs2ExpandStage2 = Bool()
    val attemptInlineImmIType = Bool()
    val buffer = FetchPacket()
  }

  object State {
    def init: State = {
      val x = State()
      x.skipIt := False
      x.immRs2ExpandStage1 := False
      x.immRs2ExpandStage2 := False
      x.attemptInlineImmIType := False
      x.buffer.assignDontCare()
      x
    }
  }
  val tmpRegIndex = 16

  val E = RvEncoding

  val input = Stream(FetchPacket())
  val output = Stream(FetchPacket())

  val exc = Machine.get[MachineException]

  def getTransform(): PostFetchTransform = {
    val pipelinedOutput = exc.resetArea {
      output.pipelined(m2s = true, s2m = true)
    }
    PostFetchTransform(input = input, output = pipelinedOutput, latency = 1)
  }

  val incomingHead = exc.valid || input.predictedBranchValid

  val state_reg = exc.resetArea { Reg(State()) init (State.init) }
  val state = incomingHead ? State.init | state_reg
  state.freeze()

  val nextState = State()
  nextState := state

  val issueIt = False
  val stallIt = False
  val issuePkt = FetchPacket()
  issuePkt.assignDontCare()

  val stateUpdate = False

  // Update state when:
  // - Stream fires (data consumed by the next stage)
  // - Our logic decides not to issue it (data consumed internally)
  when(input.valid && (input.ready || !issueIt)) {
    state_reg := nextState
    stateUpdate := True
  }

  output.valid := input.valid && issueIt
  output.payload := issuePkt
  input.ready := output.ready && !stallIt

  when(state.skipIt) {
    when(stateUpdate) {
      Machine.report(Seq("ebpf: skipIt"))
    }
    issueIt := True
    issuePkt := state.buffer
    nextState.skipIt := False
  } elsewhen (state.attemptInlineImmIType) {
    when(stateUpdate) {
      Machine.report(Seq("ebpf: attemptInlineImmIType"))
    }
    val upperIsSext = input.payload
      .insn(31 downto 12)
      .asBools
      .map(x => x ^ input.payload.insn(11))
      .orR
    when(upperIsSext) {
      issueIt := True
      issuePkt := state.buffer
      issuePkt.insn(31 downto 20) := input.payload.insn(11 downto 0)
    } otherwise {
      stallIt := True
      issueIt := True
      nextState.immRs2ExpandStage1 := True
      issuePkt := state.buffer
      issuePkt.insn.allowOverride
      issuePkt.insn := E.LUI.value
      issuePkt.insn(E.rdRange) := tmpRegIndex
      issuePkt.insn(31 downto 12) := input.payload.insn(31 downto 12)
    }
    nextState.attemptInlineImmIType := False
  } elsewhen (state.immRs2ExpandStage1) {
    when(stateUpdate) {
      Machine.report(Seq("ebpf: immRs2ExpandStage1"))
    }
    stallIt := True
    issueIt := True
    issuePkt := state.buffer
    issuePkt.insn.allowOverride
    issuePkt.insn := E.ADDI.value
    issuePkt.insn(E.rdRange) := tmpRegIndex
    issuePkt.insn(E.rs1Range) := tmpRegIndex
    issuePkt.insn(31 downto 20) := input.payload.insn(11 downto 0)
    nextState.immRs2ExpandStage1 := False
    nextState.immRs2ExpandStage2 := True
  } elsewhen (state.immRs2ExpandStage2) {
    when(stateUpdate) {
      Machine.report(Seq("ebpf: immRs2ExpandStage2"))
    }
    issueIt := True
    issuePkt := state.buffer
    issuePkt.insn.allowOverride
    issuePkt.insn(5) := True // The "not-const" bit
    issuePkt.insn(E.rs2Range) := tmpRegIndex
    nextState.immRs2ExpandStage2 := False
  } otherwise {
    // First half: control
    val opc = input.payload.insn(7 downto 0)
    val dst = input.payload.insn(11 downto 8)
    val src = input.payload.insn(15 downto 12)
    nextState.buffer := input.payload

    issueIt := True
    issuePkt := input.payload
    issuePkt.insn.allowOverride
    issuePkt.insn := 0 // Default to illegal instruction

    switch(opc) {
      is(0x07) {
        when(stateUpdate) {
          Machine.report(Seq("ebpf: add dst, imm"))
        }
        // add dst, imm
        issueIt := False
        nextState.buffer.insn := E.ADDI.value
        nextState.buffer.insn(E.rdRange) := dst.resized
        nextState.buffer.insn(E.rs1Range) := dst.resized
        nextState.attemptInlineImmIType := True
      }
      is(0x0f) {
        when(stateUpdate) {
          Machine.report(Seq("ebpf: add dst, src"))
        }
        // add dst, src
        issueIt := False
        nextState.buffer.insn := E.ADD.value
        nextState.buffer.insn(E.rdRange) := dst.resized
        nextState.buffer.insn(E.rs1Range) := dst.resized
        nextState.buffer.insn(E.rs2Range) := src.resized
        nextState.skipIt := True
      }
      is(0xa7) {
        when(stateUpdate) {
          Machine.report(Seq("ebpf: xor dst, imm"))
        }
        // xor dst, imm
        issueIt := False
        nextState.buffer.insn := E.XORI.value
        nextState.buffer.insn(E.rdRange) := dst.resized
        nextState.buffer.insn(E.rs1Range) := dst.resized
        nextState.attemptInlineImmIType := True
      }
      is(0xaf) {
        when(stateUpdate) {
          Machine.report(Seq("ebpf: xor dst, src"))
        }
        // xor dst, src
        issueIt := False
        nextState.buffer.insn := E.XOR.value
        nextState.buffer.insn(E.rdRange) := dst.resized
        nextState.buffer.insn(E.rs1Range) := dst.resized
        nextState.buffer.insn(E.rs2Range) := src.resized
        nextState.skipIt := True
      }
    }
  }
}
