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
  val parentObjects = if (inner != null) Seq(inner()) else Seq()
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
    val physSrcRegActivationMask = Flow(Vec(Bool(), spec.numPhysicalRegs))
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

  // SRC refcount
  val srcRefcountWidth = 3 bits
  def srcRefcountType = UInt(srcRefcountWidth)
  val srcRefcount = Vec(
    (0 until spec.numPhysicalRegs).map(_ => Reg(srcRefcountType) init (0))
  )
  val zeroRefcountMask = Vec(srcRefcount.map(x => x === 0))

  val incRefcount = Vec(Bool(), spec.numPhysicalRegs)
  for (b <- incRefcount) b := False

  val decRefcount = Vec(Bool(), spec.numPhysicalRegs)
  for (b <- decRefcount) b := False

  when(io.physSrcRegActivationMask.valid) {
    decRefcount := io.physSrcRegActivationMask.payload
  }

  for (i <- 0 until spec.numPhysicalRegs) {
    when(incRefcount(i) && !decRefcount(i)) {
      assert(srcRefcount(i) =/= srcRefcountType.maxValue, "Refcount overflow")
      srcRefcount(i) := srcRefcount(i) + 1
    }
    when(decRefcount(i) && !incRefcount(i)) {
      assert(srcRefcount(i) =/= 0, "Refcount underflow")
      srcRefcount(i) := srcRefcount(i) - 1
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
        val (thisAllocOk, index) = prfIf.state.findFreeReg(
          Vec(localAllowMask.zip(zeroRefcountMask).map(x => x._1 && x._2))
        )
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

  val refcountIncOk = decodeInfo.archSrcRegs
    .zip(output.physSrcRegs)
    .map { case (arch, phys) =>
      !arch.valid || srcRefcount(phys) =/= srcRefcountType.maxValue
    }
    .andR

  io.output << io.input
    .translateWith(output)
    .continueWhen(allocOk && refcountIncOk)

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
    for (
      (arch, phys) <- decodeInfo.archSrcRegs
        .zip(output.physSrcRegs)
    ) {
      when(arch.valid) {
        incRefcount(phys) := True
      }
    }
    Machine.report(
      Seq("renamed src: ") ++ decodeInfo.archSrcRegs
        .zip(output.physSrcRegs)
        .flatMap({ case (arch, phys) =>
          Seq("[v=", arch.valid, ",arch=", arch.index, ",phys=", phys, "]")
        })
    )
    Machine.report(
      Seq("renamed dst: ") ++ decodeInfo.archDstRegs
        .zip(output.physDstRegs)
        .flatMap({ case (arch, phys) =>
          Seq("[v=", arch.valid, ",arch=", arch.index, ",phys=", phys, "]")
        })
    )
  }
}

case class RenameInterface(unit: RenameUnit[_ <: PolymorphicDataChain])
    extends Bundle
    with PolymorphicDataChain {
  val parentObjects = Seq()
}
