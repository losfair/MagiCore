package miniooo

import spinal.core._
import spinal.lib._
import MiniOoOExt._

case class RenamedInsn(
    ctx: MachineContext,
    parentContextType: () => Bundle with DerefToInsn
) extends Bundle
    with DerefToInsn {
  val insn = ctx.sem.newDecodedInsn()
  val parentContext = parentContextType()
  val physSrcRegs =
    Vec(ctx.cfg.physRegIndexType, ctx.cfg.maxNumSrcRegsPerInsn)
  val physDstRegs =
    Vec(ctx.cfg.physRegIndexType, ctx.cfg.maxNumDstRegsPerInsn)
}

case class Rename(
    ctx: MachineContext,
    insnInputType: () => Bundle with DerefToInsn
) extends Area {
  case class RmtEntry() extends Bundle {
    val physRegIndex = ctx.cfg.physRegIndexType
  }

  val renamedInsnType = () => RenamedInsn(ctx, insnInputType)

  val io = new Bundle {
    val input = Stream(insnInputType())
    val output = Stream(renamedInsnType())
  }

  // Rename Map Table
  val rmt =
    Vec(
      Reg(ctx.cfg.physRegIndexType) init (0),
      ctx.cfg.numArchitecturalRegs
    )

  // Committed Map Table
  val cmt =
    Vec(
      Reg(ctx.cfg.physRegIndexType) init (0),
      ctx.cfg.numArchitecturalRegs
    )

  val output = renamedInsnType()
  output.insn := io.input.payload.insn
  output.parentContext := io.input.payload
  output.physSrcRegs := Vec(
    io.input.payload.insn.insn.archSrcRegs.map(entry => rmt(entry.index))
  )

  var currentPrfState = ctx.prfState
  var allocOk = True
  output.physDstRegs := Vec(
    io.input.payload.insn.insn.archDstRegs.map(entry => {
      val (thisAllocOk, index) = currentPrfState.findFreeReg()
      allocOk = entry.valid.mux(
        True -> (allocOk && thisAllocOk),
        False -> allocOk
      )
      currentPrfState = entry.valid.mux(
        True -> currentPrfState.markAsBusy(index),
        False -> currentPrfState
      )
      index
    })
  )

  io.output << io.input.translateWith(output).continueWhen(allocOk)

  // Speculatively write RMT update
  when(io.output.fire) {
    ctx.prfState := currentPrfState
    for (
      (arch, phys) <- io.input.payload.insn.insn.archDstRegs
        .zip(output.physDstRegs)
    ) {
      when(arch.valid) {
        rmt.write(arch.index, phys)
      }
    }
  }
}
