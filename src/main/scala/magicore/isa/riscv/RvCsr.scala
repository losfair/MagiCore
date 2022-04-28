package magicore.isa.riscv

import spinal.core._
import spinal.lib._
import magicore.util._
import MagiCoreExt._
import magicore.control._
import magicore.frontend._
import magicore.lib.funit._
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
    x.mstatus.mpie := False
    x.mstatus.mpp := RvPrivLevel.M
    x
  }
}

object RvPrivLevel extends SpinalEnum {
  val U, M = newElement()
  defaultEncoding = SpinalEnumEncoding("staticEncoding")(U -> 0, M -> 3)
}

case class RvCsrFile() extends Bundle {
  private val spec = Machine.get[MachineSpec]

  val priv = RvPrivLevel()

  val mcycle = UInt(64 bits)
  val minstret = UInt(64 bits)
  val brMiss = UInt(64 bits)
  val brHit = UInt(64 bits)
  val mscratch = UInt(spec.dataWidth)
  val mepc = UInt(spec.addrWidth)
  val mcause = UInt(spec.dataWidth)
  val mtval = UInt(spec.dataWidth)
  val mtvec = UInt(spec.addrWidth)
  val mstatus = RvMstatus()
}

case class RvMstatus() extends Bundle {
  private val intrSvc = Machine.get[RvInterruptService]
  val mpie = Bool()
  val mpp = RvPrivLevel()

  def decodeFromBits_lower(bits: Bits): RvMstatus = {
    intrSvc.mie := bits(3)
    mpie := bits(7)
    switch(bits(12 downto 11)) {
      is(B"00") {
        mpp := RvPrivLevel.U
      }
      is(B"11") {
        mpp := RvPrivLevel.M
      }
    }
    this
  }

  def encodeToBits_lower(): Bits = {
    val out = Bits(32 bits)
    out := 0
    out(3) := intrSvc.mie
    out(7) := mpie
    out(12 downto 11) := mpp.asBits
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

  val rvCsr = Machine.get[RvCsrFileReg]

  def on(
      csr: Seq[Bits],
      value: Bits,
      write: (Bits) => Unit = null,
      priv: Seq[SpinalEnumElement[RvPrivLevel.type]] = Seq(RvPrivLevel.M)
  ): Unit = {
    val accessOk = priv.map(x => rvCsr.csrFile.priv === x).orR

    when(accessOk && csr.map(x => fetch.insn(31 downto 20) === x.resized).orR) {
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
      private val mspec = Machine.get[MachineSpec]
      private val fspec = Machine.get[FrontendSpec]
      private val intrSvc = Machine.get[RvInterruptService]

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
        csr.csrFile.mcycle.asBits
      ) // cycle/mcycle
      intent.on(
        Seq(0xb02, 0xc02),
        csr.csrFile.minstret.asBits
      ) // instret/minstret

      if (mspec.dataWidth.value == 32) {
        intent.on(
          Seq(0xb80, 0xc80),
          csr.csrFile.mcycle(63 downto 32).asBits
        ) // cycleh/mcycleh
        intent.on(
          Seq(0xb82, 0xc82),
          csr.csrFile.minstret(63 downto 32).asBits
        ) // instreth/minstreth
      }
      // brmiss
      intent.on(
        Seq(0xc03),
        csr.csrFile.brMiss.asBits
      )
      if (mspec.dataWidth.value == 32) {
        intent.on(
          Seq(0xc83),
          csr.csrFile.brMiss(63 downto 32).asBits
        )
      }

      // brhit
      intent.on(
        Seq(0xc04),
        csr.csrFile.brHit.asBits
      )
      if (mspec.dataWidth.value == 32) {
        intent.on(
          Seq(0xc84),
          csr.csrFile.brHit(63 downto 32).asBits
        )
      }

      // mstatus
      intent.on(
        Seq(0x300),
        csr.csrFile.mstatus.encodeToBits_lower(),
        x => csr.csrFile.mstatus.decodeFromBits_lower(x)
      )

      if (mspec.dataWidth.value == 32) {
        // mstatush
        intent.on(
          Seq(0x310),
          0
        )
      }

      // mtvec
      intent.on(
        Seq(0x305),
        csr.csrFile.mtvec.asBits,
        x => csr.csrFile.mtvec := x.asUInt.resized
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
        x => csr.csrFile.mepc := x.asUInt.resized
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

      // misa
      val misa = Bits(32 bits)
      misa := 0
      misa(20) := True // U
      misa(12) := True // M
      misa(8) := True // I
      intent.on(
        Seq(0x301),
        misa
      )

      // mvendorid
      intent.on(
        Seq(0xf11),
        0
      )

      // marchid
      intent.on(
        Seq(0xf12),
        0
      )

      // mimpid
      intent.on(
        Seq(0xf13),
        0
      )

      // mhartid
      intent.on(
        Seq(0xf14),
        0
      )

      // mip
      intent.on(
        Seq(0x344),
        intrSvc.csrMip.encoding
      )

      // mie
      intent.on(
        Seq(0x304),
        intrSvc.csrMie.encoding,
        intrSvc.csrMie.decodeAndSet
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
            csr.csrFile.mcause.getWidth - 1 bits
          )).asUInt
          csr.csrFile.mepc := fetch.pc
          csr.csrFile.mtval := mtval.resized
          // 3.1.6.1
          // When a trap is taken from privilege mode y into privilege mode x,
          // x PIE is set to the value of x IE; x IE is set to 0; and x PP is set to y.
          csr.csrFile.mstatus.mpp := csr.csrFile.priv
          csr.csrFile.mstatus.mpie := intrSvc.mie
          intrSvc.mie := False
          csr.csrFile.priv := RvPrivLevel.M
        }

        val wfiPending = Reg(Bool()) init (false)
        val serializePending = Reg(Bool()) init (false)
        val pendingPC = Reg(fspec.addrType())
        val pendingPC_next = pendingPC + 4

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
              when(intrSvc.trigger) {
                restartIntoException(intrSvc.cause, 0, true)
              } otherwise {
                // spurious
                restartIt_reg := True
                restartPC_reg := fetch.pc
              }
            }
            is(MachineExceptionCode.EXCEPTION_RETURN) {
              // MRET
              // 8.6.4 Trap Return
              restartIt_reg := True
              restartPC_reg := csr.csrFile.mepc
              csr.csrFile.priv := csr.csrFile.mstatus.mpp

              // MRET then in mstatus/mstatush sets MPV=0, MPP=0,
              // MIE=MPIE, and MPIE=1. Lastly, MRET sets the privilege mode as previously determined, and
              // sets pc=mepc.
              csr.csrFile.mstatus.mpp := RvPrivLevel.U
              intrSvc.mie := csr.csrFile.mstatus.mpie
              csr.csrFile.mstatus.mpie := True
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
            is(MachineExceptionCode.WFI) {
              wfiPending := True
              pendingPC := fetch.pc
            }
            is(MachineExceptionCode.SERIALIZE) {
              serializePending := True
              pendingPC := fetch.pc
            }
            default {}
          }
        }

        when(wfiPending && intrSvc.active) {
          wfiPending := False
          restartIt_reg := True
          restartPC_reg := pendingPC_next
        }

        val serializeLogic = new Area {
          // Our own control signal is delayed for at least 1 cycle so delaying `lsuBusy` by one cycle is valid here
          val lsuBusy = RegNext(Machine.get[LsuBusySignal].busy, True)
          when(serializePending && !lsuBusy) {
            serializePending := False
            restartIt_reg := True
            restartPC_reg := pendingPC_next
          }
        }

        Machine.provide(
          FetchRestartSignal(valid = restartIt_reg, pc = restartPC_reg)
        )
      }
    }
  }

}
