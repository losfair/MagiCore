package magicore.control

import spinal.core._
import spinal.lib._
import magicore.util._

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
              .zip(cmtAllowMask)
              .map(x => x._1 && x._2)
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


  io.output << io.input
    .translateWith(output)
    .continueWhen(allocOk)
  when(io.input.isStall) {
    Machine.report(
      Seq("Rename STALL: allocOk=", allocOk)
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
    Machine.report(
      Seq("renamed src: ") ++ decodeInfo.archSrcRegs
        .zip(output.physSrcRegs)
        .flatMap({ case (arch, phys) =>
          Seq("[v=", arch.valid, ",wait=", arch.waitValue, ",arch=", arch.index, ",phys=", phys, "]")
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
