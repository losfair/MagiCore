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

case class RenameUnit[T <: PolymorphicDataChain](
    dataType: HardType[T],
    reset: Bool
) extends Area {
  private val spec = Machine.get[MachineSpec]
  case class RmtEntry() extends Bundle {
    val physRegIndex = spec.physRegIndexType
  }

  def reportXmt(name: String, x: Vec[UInt]) {
    Machine.report(
      Seq("Mapping table " + name + ": ") ++ x.zipWithIndex.flatMap(y =>
        Seq("[" + y._2 + "]=", y._1, " ")
      )
    )
  }

  def validateXmt(name: String, xmt: Vec[UInt], allowMask: Vec[Bool]) {
    for (i <- 0 until spec.numArchitecturalRegs) {
      assert(
        xmt(i) === 0 || allowMask(xmt(i)) === False,
        name + " index " + i + " is in the table but masked"
      )
      for (j <- i + 1 until spec.numArchitecturalRegs) {
        assert(xmt(i) === 0 || xmt(i) =/= xmt(j), name + " is not a bijection")
      }
    }
    for (i <- 0 until spec.numPhysicalRegs) {
      assert(
        allowMask(i) || xmt.map(x => x === i).orR,
        name + " physreg " + i + " is masked but not in the table"
      )
    }
  }

  def outType = new RenameInfo(dataType())

  val io = new Bundle {
    val input = Stream(dataType())
    val output = Stream(outType)
    val physSrcRegActivationMask_ooo = Flow(Vec(Bool(), spec.numPhysicalRegs))
    val physSrcRegActivationMask_ino = Flow(Vec(Bool(), spec.numPhysicalRegs))
  }

  // Rename Map Table
  val rmt =
    Vec(
      Reg(spec.physRegIndexType) init (0),
      spec.numArchitecturalRegs
    )
  val rmtAllowMask = Vec(Reg(Bool()) init (true), spec.numPhysicalRegs)

  // Committed Map Table
  val cmt =
    Vec(
      Reg(spec.physRegIndexType) init (0),
      spec.numArchitecturalRegs
    )
  val cmtAllowMask = Vec(Reg(Bool()) init (true), spec.numPhysicalRegs)

  validateXmt("rmt", rmt, rmtAllowMask)
  validateXmt("cmt", cmt, cmtAllowMask)

  // SRC refcount
  val srcRefcountWidth = 3 bits
  def srcRefcountType = UInt(srcRefcountWidth)
  val srcRefcount = new ResetArea(reset = reset, cumulative = true) {
    val v = Vec(
      (0 until spec.numPhysicalRegs).map(_ => Reg(srcRefcountType) init (0))
    )
  }.v
  val zeroRefcountMask = Vec(srcRefcount.map(x => x === 0))

  val incRefcount = Vec(Bool(), spec.numPhysicalRegs)
  for (b <- incRefcount) b := False

  val decRefcount_ooo = Vec(Bool(), spec.numPhysicalRegs)
  for ((b, i) <- decRefcount_ooo.zipWithIndex)
    b := io.physSrcRegActivationMask_ooo.valid & io.physSrcRegActivationMask_ooo
      .payload(i)

  val decRefcount_ino = Vec(Bool(), spec.numPhysicalRegs)
  for ((b, i) <- decRefcount_ino.zipWithIndex)
    b := io.physSrcRegActivationMask_ino.valid & io.physSrcRegActivationMask_ino
      .payload(i)

  for (i <- 0 until spec.numPhysicalRegs) {
    val (inc, dec1, dec2) =
      (incRefcount(i), decRefcount_ooo(i), decRefcount_ino(i))
    when(inc && !dec1 && !dec2) {
      assert(srcRefcount(i) =/= srcRefcountType.maxValue, "Refcount overflow")
      srcRefcount(i) := srcRefcount(i) + 1
    }
    when((inc && dec1 && dec2) || (!inc && (dec1 ^ dec2))) {
      assert(srcRefcount(i) =/= 0, "Refcount underflow (1)")
      srcRefcount(i) := srcRefcount(i) - 1
    }
    when(!inc && dec1 && dec2) {
      assert(
        srcRefcount(i) =/= 0 && srcRefcount(i) =/= 1,
        "Refcount underflow (2)"
      )
      srcRefcount(i) := srcRefcount(i) - 2
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
  ({
    var localAllowMask = rmtAllowMask
    output.physDstRegs := Vec(
      decodeInfo.archDstRegs.map(entry => {
        val allocatedIndex = spec.physRegIndexType
        val (thisAllocOk, index) = prfIf.state.findFreeReg(
          Vec(
            localAllowMask
              .zip(zeroRefcountMask)
              .zip(cmtAllowMask)
              .map(x => x._1._1 && x._1._2 && x._2)
          )
        )
        allocOk = entry.valid.mux(
          True -> (allocOk && thisAllocOk),
          False -> allocOk
        )
        localAllowMask = rmtAllowMask.clone()
        when(entry.valid) {
          localAllowMask(index) := False
          when(io.output.fire) {
            prfIf.state.markAsBusyInPlace(index)
          }
          allocatedIndex := index
        } otherwise {
          allocatedIndex := 0
        }
        allocatedIndex
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
  when(io.input.isStall) {
    Machine.report(
      Seq("Rename STALL: allocOk=", allocOk, " refcountIncOk=", refcountIncOk)
    )
  }

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

  // Ensure that this logic has higher priority than `DispatchUnit`.
  Component.current.afterElaboration {
    when(reset) {
      rmt := cmt
      rmtAllowMask := cmtAllowMask
      reportXmt("rmt.reset", cmt)
    }
  }
}

case class RenameInterface(unit: RenameUnit[_ <: PolymorphicDataChain])
    extends Bundle
    with PolymorphicDataChain {
  val parentObjects = Seq()
}
