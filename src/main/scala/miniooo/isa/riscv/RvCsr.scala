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
    x.mcycle := 0
    x.minstret := 0
    x
  }
}

case class RvCsrFile() extends Bundle {
  val mcycle = UInt(64 bits)
  val minstret = UInt(64 bits)
}

case class RvCsrFileReg() extends Area {
  val csrFile = Reg(RvCsrFile()) init (RvCsrFile.init)

  csrFile.mcycle := csrFile.mcycle + 1

  def provide() {
    Machine.provide(DispatchPerfCounters(instRetired = csrFile.minstret))
    Machine.provide(this)
  }
}

case class RvCsrFileIntent(data: PolymorphicDataChain) extends Area {
  private val mspec = Machine.get[MachineSpec]
  val fetch = data.lookup[FetchPacket]
  val issue = data.lookup[IssuePort[_]]

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
          if (write != null) write(src)
          out := value.resized
          ok := True
        }
        is(B"10") {
          // CSRRS
          val newValue = value | src.resized
          if (write != null) write(newValue)
          out := value.resized
          ok := True
        }
        is(B"11") {
          // CSRRC
          val newValue = value & (~src).resized
          if (write != null) write(newValue)
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
      val io_available = null
      val io_input = Stream(hardType())
      val io_output = Stream(CommitRequest(null))

      val csr = Machine.get[RvCsrFileReg]

      val buffered_input = io_input.pipelined(m2s = true, s2m = true)
      val buffered_output = Stream(CommitRequest(null))
      io_output << buffered_output.pipelined(m2s = true)

      val intent = RvCsrFileIntent(buffered_input.payload)
      val commit = CommitRequest(null)

      commit.token := buffered_input.payload.lookup[CommitToken]
      commit.regWriteValue(0) := intent.out
      commit.exception.assignDontCare()
      commit.exception.code := MachineExceptionCode.DECODE_ERROR
      commit.exception.valid := !intent.ok
      buffered_output << buffered_input.translateWith(commit)

      intent.on(Seq(0xb00, 0xc00), csr.csrFile.mcycle(31 downto 0).asBits) // cycle/mcycle
      intent.on(Seq(0xb02, 0xc02), csr.csrFile.minstret(31 downto 0).asBits) // instret/minstret
      intent.on(Seq(0xb80, 0xc80), csr.csrFile.mcycle(63 downto 32).asBits) // cycleh/mcycleh
      intent.on(Seq(0xb82, 0xc82), csr.csrFile.minstret(63 downto 32).asBits) // instreth/minstreth
    }
  }

}
