package miniooo.control

import spinal.core._
import spinal.lib._
import miniooo.util._

case class RenameInfo(inner: HardType[_ <: PolymorphicDataChain])
    extends Bundle
    with PolymorphicDataChain {
  private val spec = Machine.get[MachineSpec]

  val physSrcRegs =
    Vec(spec.physRegIndexType, spec.maxNumSrcRegsPerInsn)
  val physDstRegs =
    Vec(spec.physRegIndexType, spec.maxNumDstRegsPerInsn)
  val parentObjects = if(inner != null) Seq(inner()) else Seq()
}

case class RenameUnit[T <: PolymorphicDataChain](dataType: HardType[T])
    extends Area {
  private val spec = Machine.get[MachineSpec]
  case class RmtEntry() extends Bundle {
    val physRegIndex = spec.physRegIndexType
  }

  def outType = new RenameInfo(dataType())

  val io = new Bundle {
    val input = Stream(dataType())
    val output = Stream(outType)
  }

  // Rename Map Table
  val rmt =
    Vec(
      Reg(spec.physRegIndexType) init (0),
      spec.numArchitecturalRegs
    )
  val rmtAllowMask = Vec(Reg(Bool()) init (true), spec.numPhysicalRegs)

  // Check RMT invariants
  for (i <- 0 until spec.numArchitecturalRegs) {
    assert(rmt(i) === 0 || rmtAllowMask(rmt(i)) === False)
    for (j <- i + 1 until spec.numArchitecturalRegs) {
      assert(rmt(i) === 0 || rmt(i) =/= rmt(j), "RMT is not a bijection")
    }
  }

  // Committed Map Table
  val cmt =
    Vec(
      Reg(spec.physRegIndexType) init (0),
      spec.numArchitecturalRegs
    )
  val cmtAllowMask = Vec(Reg(Bool()) init (true), spec.numPhysicalRegs)

  // Check CMT invariants
  for (i <- 0 until spec.numArchitecturalRegs) {
    assert(cmt(i) === 0 || cmtAllowMask(cmt(i)) === False)
    for (j <- i + 1 until spec.numArchitecturalRegs) {
      assert(cmt(i) === 0 || cmt(i) =/= cmt(j), "CMT is not a bijection")
    }
  }

  val prfIf = Machine.get[PrfInterface]
  val decodeInfo = io.input.payload.lookup[DecodeInfo]

  val output = outType
  output.parentObjects(0) := io.input.payload
  output.physSrcRegs := Vec(
    decodeInfo.archSrcRegs.map(entry => rmt(entry.index))
  )

  var allocOk = True
  val applyRmtUpdate = False
  ({
    var localAllowMask = rmtAllowMask
    output.physDstRegs := Vec(
      decodeInfo.archDstRegs.map(entry => {
        val (thisAllocOk, index) = prfIf.state.findFreeReg(localAllowMask)
        allocOk = entry.valid.mux(
          True -> (allocOk && thisAllocOk),
          False -> allocOk
        )
        localAllowMask = localAllowMask.clone()
        localAllowMask(index) := False
        when(io.output.fire) {
          prfIf.state.markAsBusyInPlace(index)
        }
        index
      })
    )
  })

  io.output << io.input.translateWith(output).continueWhen(allocOk)

  // Speculatively write RMT update
  when(io.output.fire) {
    for (
      (arch, phys) <- decodeInfo.archDstRegs
        .zip(output.physDstRegs)
    ) {
      when(arch.valid) {
        rmt.write(arch.index, phys)
        rmtAllowMask.write(rmt(arch.index), True)
        rmtAllowMask.write(phys, False)
      }
    }
  }
}

case class RenameInterface(unit: RenameUnit[_ <: PolymorphicDataChain])
    extends Bundle
    with PolymorphicDataChain {
  val parentObjects = Seq()
}
