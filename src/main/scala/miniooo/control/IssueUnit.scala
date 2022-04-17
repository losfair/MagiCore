package miniooo.control

import spinal.core._
import spinal.lib._
import miniooo.util._
import miniooo.util.MiniOoOExt._

case class IssueConfig(
    portSpecs: Seq[IssueSpec]
) {}

case class IssuePort[T <: Data](hardType: HardType[T])
    extends Bundle
    with PolymorphicDataChain {
  val parentObjects = Seq(hardType())
  def data = parentObjects(0)

  val srcRegData = Vec(
    Machine.get[MachineSpec].dataType,
    Machine.get[MachineSpec].maxNumSrcRegsPerInsn
  )

  def toPhysSrcRegActivationMask(): Vec[Bool] = {
    val spec = Machine.get[MachineSpec]
    val renameInfo = this.lookup[RenameInfo]
    val decodeInfo = this.lookup[DecodeInfo]

    Vec(
      (0 until spec.numPhysicalRegs).map(i =>
        decodeInfo.archSrcRegs
          .zip(renameInfo.physSrcRegs)
          .map({
            case (arch, phys) => {
              arch.valid && phys === i
            }
          })
          .orR
      )
    )
  }
}

case class IssueSpec(
    staticTag: Data
)

case class IssueQueue[T <: PolymorphicDataChain](
    c: IssueConfig,
    dataType: HardType[T]
) extends Area {
  private val spec = Machine.get[MachineSpec]
  private val prf = Machine.get[PrfInterface]
  val indexSize = log2Up(spec.issueQueueSize) bits
  def indexType = UInt(indexSize)

  case class IqDependency() extends Bundle {
    val valid = Bool()
    val wakeUp = Bool()
    val physRegIndex = UInt(log2Up(spec.numPhysicalRegs) bits)
  }

  object IqDependency {
    def idle: IqDependency = {
      val dep = IqDependency()
      dep.valid := False
      dep.wakeUp.assignDontCare()
      dep.physRegIndex.assignDontCare()
      dep
    }
  }

  case class IqTag() extends Bundle {
    val valid = Bool()
    val priority = UInt(log2Up(spec.issueQueueSize) bits)
    val dependencies = Vec(IqDependency(), spec.maxNumSrcRegsPerInsn)
    val portIndex = UInt(log2Up(c.portSpecs.size) bits)

    def canIssue = valid && dependencies.map(x => !x.valid || x.wakeUp).andR
  }

  object IqTag {
    def idle: IqTag = {
      val tag = IqTag()
      tag.valid := False
      tag.portIndex.assignDontCare()
      tag.priority.assignDontCare()
      for (i <- 0 until spec.maxNumSrcRegsPerInsn) {
        tag.dependencies(i) := IqDependency.idle
      }
      tag
    }
  }

  case class IqData() extends Bundle {
    val data = dataType()
  }

  println("IqTag width: " + IqTag().getBitsWidth)

  val iqTagSpace = Vec(Reg(IqTag()) init (IqTag.idle), spec.issueQueueSize)
  val physRegBusyMask: Vec[Bool] = Vec(
    (0 until spec.numPhysicalRegs).map(i =>
      iqTagSpace
        .map(x =>
          x.valid && x.dependencies
            .map(y => y.valid && y.physRegIndex === i)
            .orR
        )
        .orR
    )
  )

  // Wakeup logic
  for (t <- iqTagSpace) {
    for (dep <- t.dependencies) {
      val wakeUp = prf.listen(dep.physRegIndex)
      dep.wakeUp := dep.wakeUp | wakeUp
    }
  }

  val incPriority = Bool()
  when(incPriority) {
    for (t <- iqTagSpace) {
      when(t.priority =/= t.priority.maxValue) {
        t.priority := t.priority + 1
      }
    }
  }

  // Verification
  {
    val prev = Vec(iqTagSpace.map(x => RegNext(next = x, init = IqTag.idle)))
    for ((prev, next) <- prev.zip(iqTagSpace)) {
      when(prev.valid && !prev.canIssue && !next.valid) {
        assert(False, "detected invalid issue event")
      }
    }
  }

  def push(enable: Bool, data: T): (Bool, UInt) = {
    val t = IqTag()
    val renameInfo = data.lookup[RenameInfo]
    val decodeInfo = data.lookup[DecodeInfo]

    t.valid := True
    t.priority := 0
    t.dependencies := Vec(
      decodeInfo.archSrcRegs
        .map(_.valid)
        .zip(renameInfo.physSrcRegs)
        .map(arg => {
          val (valid, physRegIndex) = arg
          val out = IqDependency()
          out.valid := valid

          // Initial wakeup
          out.wakeUp := prf.state.table(physRegIndex).dataAvailable | prf.listen(physRegIndex)

          out.physRegIndex := physRegIndex
          out
        })
    )

    val (portIndexOk, portIndex) = (0 until c.portSpecs.size).firstWhere(
      hardType = UInt(log2Up(c.portSpecs.size) bits),
      predicate = i => c.portSpecs(i).staticTag === decodeInfo.functionUnitTag,
      generate = x => U(x, log2Up(c.portSpecs.size) bits)
    )
    assert(!enable || portIndexOk, "invalid port index")
    t.portIndex := portIndex

    val iqFreeMask = SetFromFirstOne(Vec(iqTagSpace.map(!_.valid)))
    val notFull = iqFreeMask.orR

    val iqDataAddr = UInt(log2Up(spec.issueQueueSize) bits)
    iqDataAddr.assignDontCare()

    var allocated = U(0, 32 bits)

    for (i <- 0 until spec.issueQueueSize) {
      val first = i == 0
      val cond =
        if (first) iqFreeMask(i) else (iqFreeMask(i) =/= iqFreeMask(i - 1))
      allocated = cond.mux(
        False -> allocated,
        True -> (allocated + 1)
      )
      when(cond) {
        assert(!iqTagSpace(i).valid, "refusing to overwrite IQ entry")
        iqDataAddr := i
        when(enable) {
          iqTagSpace(i) := t
        }
      }
    }

    incPriority := notFull & enable

    assert(
      (notFull && allocated === 1) || (!notFull && allocated === 0),
      Seq("invalid IQ allocation notFull=", notFull, " allocated=", allocated)
    )
    (notFull & enable, iqDataAddr)
  }

  def report(): Seq[Any] = {
    iqTagSpace.zipWithIndex.flatMap({ case (x, i) =>
      Seq(
        if (i % 4 == 0) "\\n" else "",
        "\\t",
        i.toString(),
        ": ",
        "v=",
        x.valid,
        " i=",
        x.canIssue,
        " prio=",
        x.priority,
        " deps=["
      ) ++ x.dependencies.flatMap(y =>
        Seq("(", "v=", y.valid, " idx=", y.physRegIndex, " w=", y.wakeUp, ")")
      ) ++ Seq("]")
    })
  }

  def queryPop(issueAvailable: Vec[Bool]): (Bool, UInt) = {
    val (_, index, ok) = iqTagSpace.zipWithIndex
      .map({ case (x, i) =>
        (x.priority, U(i, indexSize), x.canIssue && issueAvailable(x.portIndex))
      })
      .reduceBalancedTree((l, r) => {
        val prio = UInt(l._1.getWidth bits)
        val index = indexType
        when(l._3 && r._3) {
          when(l._1 <= r._1) {
            prio := l._1
            index := l._2
          } otherwise {
            prio := r._1
            index := r._2
          }
        } elsewhen (l._3) {
          prio := l._1
          index := l._2
        } otherwise {
          prio := r._1
          index := r._2
        }

        (prio, index, l._3 | r._3)
      })

    (ok, index)
  }

  def commitPop(index: UInt) {
    assert(iqTagSpace(index).valid, "invalid IQ pop")
    iqTagSpace(index).valid := False
  }
}

case class IssueUnit[T <: PolymorphicDataChain](
    c: IssueConfig,
    dataType: HardType[T]
) extends Area {
  private val spec = Machine.get[MachineSpec]
  private val prf = Machine.get[PrfInterface]

  case class IqData() extends Bundle {
    val data = dataType()
  }

  val issueDataType = IssuePort(dataType)

  val io = new Bundle {
    val input = Stream(dataType())
    val issuePorts = Vec(Stream(issueDataType), c.portSpecs.size)
    val issueAvailable = Vec(Bool(), c.portSpecs.size)
    val issueMonitor = Flow(issueDataType)
  }

  val iq = IssueQueue(c = c, dataType = dataType)
  val iqDataSpace = Mem(IqData(), spec.issueQueueSize)
  println("IqData width: " + IqData().getBitsWidth)

  val issuePushLogic = new Area {
    val newIqData = IqData()
    newIqData.data := io.input.payload

    val (fire, iqDataAddr) =
      iq.push(enable = io.input.valid, data = io.input.payload)
    io.input.ready := fire
    iqDataSpace.write(
      address = iqDataAddr,
      data = newIqData,
      enable = fire
    )
  }

  val issuePopLogic = new Area {
    val commit = Bool()
    val (nextReady, nextIndex) = iq.queryPop(io.issueAvailable)
    val keep = Reg(Bool()) init (false) setWhen (nextReady) clearWhen (commit)
    val rReady = Reg(Bool())
    val rIndex = Reg(iq.indexType)

    val ready = keep ? rReady | nextReady
    val index = keep ? rIndex | nextIndex
    rReady := ready
    rIndex := index

    when(commit) {
      iq.commitPop(index)
    }

    assert(
      (nextReady && ready) || (!nextReady && !ready),
      "control status changed"
    ) // data might be different but control status must not change

    val iqContent =
      iqDataSpace.readAsync(address = index, readUnderWrite = writeFirst)
    val renameInfo = iqContent.data.lookup[RenameInfo]
    val decodeInfo = iqContent.data.lookup[DecodeInfo]

    val srcRegContent =
      renameInfo.physSrcRegs.map(x => prf.readAsync(x))

    val unifiedIssuePort = Stream(issueDataType)
    unifiedIssuePort.setBlocked()
    unifiedIssuePort.valid := ready
    unifiedIssuePort.payload.srcRegData := Vec(
      srcRegContent.map(x => x.data)
    )
    unifiedIssuePort.payload.data := iqContent.data
    commit := unifiedIssuePort.fire

    val issueOk = Vec(Bool(), io.issuePorts.size)
    for (b <- issueOk) b := False

    for ((fu, fuIndex) <- io.issuePorts.zipWithIndex) {
      fu.check()
      fu.valid := False
      fu.payload := unifiedIssuePort.payload

      when(
        decodeInfo.functionUnitTag === c.portSpecs(fuIndex).staticTag
      ) {
        fu.valid := unifiedIssuePort.valid
        unifiedIssuePort.ready := fu.ready
        issueOk(fuIndex) := True
      }
    }

    when(unifiedIssuePort.valid) {
      var issueCount = U(0, 32 bits)
      for (b <- issueOk) {
        issueCount = issueCount + b.asUInt.resized
      }
      assert(issueCount === 1, "issue count mismatch")
    }

    when(unifiedIssuePort.fire) {
      Machine.report(Seq("issued - mask ", issueOk.asBits, " iq index ", index))
      try {
        val dispatchInfo = iqContent.data.lookup[DispatchInfo]
        Machine.report(Seq("issued rob index is ", dispatchInfo.robIndex))
      } catch {
        case _: Exception => {}
      }
      Machine.report(iq.report())
    }

    io.issueMonitor << unifiedIssuePort.asFlow.throwWhen(
      !unifiedIssuePort.ready
    )
  }
}
