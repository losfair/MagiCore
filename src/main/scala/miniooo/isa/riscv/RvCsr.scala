package miniooo.isa.riscv

import spinal.core._
import spinal.lib._
import miniooo.util._
import MiniOoOExt._
import miniooo.control._
import miniooo.frontend._
import miniooo.lib.funit._
import spinal.lib.bus.amba4.axi._
import spinal.lib.fsm._

object RvCsrFile {
  def init: RvCsrFile = {
    val x = RvCsrFile()
    x.priv := RvPrivLevel.M
    x.mcycle := 0
    x.minstret := 0
    x.brMiss := 0
    x.brHit := 0
    x.mscratch := 0
    x.mepc := 0
    x.mcause := 0
    x.mtval := 0
    x.mtvec := 0
    x.mstatus.mie := False
    x.mstatus.mpie := False
    x.mstatus.mpp := 0
    x
  }
}

object RvPrivLevel extends SpinalEnum(binarySequential) {
  val U, M = newElement()
}

case class RvCsrFile() extends Bundle {
  val priv = RvPrivLevel()

  val mcycle = UInt(64 bits)
  val minstret = UInt(64 bits)
  val brMiss = UInt(64 bits)
  val brHit = UInt(64 bits)
  val mscratch = UInt(32 bits)
  val mepc = UInt(32 bits)
  val mcause = UInt(32 bits)
  val mtval = UInt(32 bits)
  val mtvec = UInt(32 bits)
  val mstatus = RvMstatus()
}

case class RvMstatus() extends Bundle {
  val mie = Bool()
  val mpie = Bool()
  val mpp = Bits(2 bits)

  def decodeFromBits_lower(bits: Bits): RvMstatus = {
    mie := bits(3)
    mpie := bits(7)
    mpp := bits(12 downto 11)
    this
  }

  def encodeToBits_lower(): Bits = {
    val out = Bits(32 bits)
    out := 0
    out(3) := mie
    out(7) := mpie
    out(12 downto 11) := mpp
    out
  }
}

case class RvCsrFileReg() extends Area {
  val csrFile = Reg(RvCsrFile()) init (RvCsrFile.init)

  csrFile.mcycle := csrFile.mcycle + 1

  def provide() {
    Machine.provide(DispatchPerfCounters(instRetired = csrFile.minstret))
    Machine.provide(
      AluPerfCounters(brMiss = csrFile.brMiss, brHit = csrFile.brHit)
    )
    Machine.provide(this)
  }
}

case class RvCsrFileIntent(data: Stream[_ <: PolymorphicDataChain])
    extends Area {
  private val mspec = Machine.get[MachineSpec]

  val fetch = data.payload.lookup[FetchPacket]
  val issue = data.payload.lookup[IssuePort[_]]

  val useConst = fetch.insn(14)
  val src =
    useConst ? fetch.insn(19 downto 15).resize(mspec.dataWidth) | issue
      .srcRegData(0)
  val op = fetch.insn(13 downto 12)

  val out = Bits(mspec.dataWidth)
  out.assignDontCare()

  val ok = False

  def on(
      csr: Seq[Bits],
      value: Bits,
      write: (Bits) => Unit = null
  ): Unit = {
    when(csr.map(x => fetch.insn(31 downto 20) === x.resized).orR) {
      switch(op) {
        is(B"01") {
          // CSRRW
          if (write != null) when(data.fire) { write(src) }
          out := value.resized
          ok := True
        }
        is(B"10") {
          // CSRRS
          val newValue = value | src.resized
          if (write != null) when(data.fire) { write(newValue) }
          out := value.resized
          ok := True
        }
        is(B"11") {
          // CSRRC
          val newValue = value & (~src).resized
          if (write != null) when(data.fire) { write(newValue) }
          out := value.resized
          ok := True
        }
      }
    }
  }
}

class RvCsr(staticTagData: => Data) extends FunctionUnit {

  def staticTag: Data = staticTagData

  override def inOrder: Boolean = true

  // So that we can apply the effect directly in the pipeline instead of waiting for writeback effect
  override def inOrder_sideEffect: Boolean = true

  def generate(
      hardType: HardType[_ <: PolymorphicDataChain]
  ): FunctionUnitInstance = {
    new FunctionUnitInstance {
      private val fspec = Machine.get[FrontendSpec]
      val io_available = null
      val io_input = Stream(hardType())
      val io_output = Stream(CommitRequest(null))

      val csr = Machine.get[RvCsrFileReg]

      val buffered_input = io_input.pipelined(m2s = true, s2m = true)
      val buffered_output = Stream(CommitRequest(null))
      io_output << buffered_output.pipelined(m2s = true)

      val intent = RvCsrFileIntent(buffered_input)
      val commit = CommitRequest(null)

      commit.token := buffered_input.payload.lookup[CommitToken]
      commit.regWriteValue(0) := intent.out
      commit.exception.assignDontCare()
      commit.exception.code := MachineExceptionCode.DECODE_ERROR
      commit.exception.valid := !intent.ok
      buffered_output << buffered_input.translateWith(commit)

      intent.on(
        Seq(0xb00, 0xc00),
        csr.csrFile.mcycle(31 downto 0).asBits
      ) // cycle/mcycle
      intent.on(
        Seq(0xb02, 0xc02),
        csr.csrFile.minstret(31 downto 0).asBits
      ) // instret/minstret
      intent.on(
        Seq(0xb80, 0xc80),
        csr.csrFile.mcycle(63 downto 32).asBits
      ) // cycleh/mcycleh
      intent.on(
        Seq(0xb82, 0xc82),
        csr.csrFile.minstret(63 downto 32).asBits
      ) // instreth/minstreth

      // brmiss
      intent.on(
        Seq(0xc03),
        csr.csrFile.brMiss(31 downto 0).asBits
      )
      intent.on(
        Seq(0xc83),
        csr.csrFile.brMiss(63 downto 32).asBits
      )

      // brhit
      intent.on(
        Seq(0xc04),
        csr.csrFile.brHit(31 downto 0).asBits
      )
      intent.on(
        Seq(0xc84),
        csr.csrFile.brHit(63 downto 32).asBits
      )

      // mstatus
      intent.on(
        Seq(0x300),
        csr.csrFile.mstatus.encodeToBits_lower(),
        x => csr.csrFile.mstatus.decodeFromBits_lower(x)
      )

      // mstatush
      intent.on(
        Seq(0x310),
        0
      )

      // mtvec
      intent.on(
        Seq(0x305),
        csr.csrFile.mtvec.asBits,
        x => csr.csrFile.mtvec := x.asUInt
      )

      // mscratch
      intent.on(
        Seq(0x340),
        csr.csrFile.mscratch.asBits,
        x => csr.csrFile.mscratch := x.asUInt
      )

      // mepc
      intent.on(
        Seq(0x341),
        csr.csrFile.mepc.asBits,
        x => csr.csrFile.mepc := x.asUInt
      )

      // mcause
      intent.on(
        Seq(0x342),
        csr.csrFile.mcause.asBits,
        x => csr.csrFile.mcause := x.asUInt
      )

      // mtval
      intent.on(
        Seq(0x343),
        csr.csrFile.mtval.asBits,
        x => csr.csrFile.mtval := x.asUInt
      )

      // Exception handling
      val exceptionLogic = new Area {
        val restartIt_reg = Reg(Bool()) init (false)
        restartIt_reg := False

        val restartPC_reg = Reg(fspec.addrType())

        val fullExc = Machine.get[FullMachineException]
        val exc = fullExc.exc
        val fetch = fullExc.lookup[FetchPacket]

        val excRestartPC = (csr.csrFile.mtvec(
          csr.csrFile.mtvec.getWidth - 1 downto 2
        ) ## B"00").asUInt
        def restartIntoException(
            mcause: UInt,
            mtval: UInt,
            interrupt: Boolean = false
        ) {
          restartIt_reg := True
          restartPC_reg := excRestartPC
          csr.csrFile.mcause := (Bool(interrupt).asBits ## mcause.resize(
            31 bits
          )).asUInt
          csr.csrFile.mepc := fetch.pc
          csr.csrFile.mtval := mtval
        }

        when(exc.valid) {
          switch(exc.code) {
            is(MachineExceptionCode.DECODE_ERROR) {
              // Illegal instruction
              restartIntoException(2, fetch.insn.asUInt)
            }
            is(MachineExceptionCode.MEMORY_ERROR) {
              // Load access fault
              restartIntoException(5, exc.memoryError_accessAddr.asUInt)
            }
            is(MachineExceptionCode.EXT_INTERRUPT) {
              // External interrupt.
              // TODO: Vectored interrupt
              restartIntoException(exc.extInterrupt_cause.asUInt, 0, true)
            }
            is(MachineExceptionCode.EXCEPTION_RETURN) {
              // MRET
              restartIt_reg := True
              restartPC_reg := csr.csrFile.mepc
            }
            is(MachineExceptionCode.ENV_CALL) {
              // ecall
              restartIntoException(
                csr.csrFile.priv.mux(
                  RvPrivLevel.U -> U(8, 4 bits),
                  RvPrivLevel.M -> U(11, 4 bits)
                ),
                0
              )
            }
            default {}
          }
        }

        Machine.provide(
          FetchRestartSignal(valid = restartIt_reg, pc = restartPC_reg)
        )
      }
    }
  }

}
