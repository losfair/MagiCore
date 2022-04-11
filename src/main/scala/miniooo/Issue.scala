package miniooo

import spinal.core._
import spinal.lib._

case class IssuePort(ctx: MachineContext) extends Bundle {
  val insn = IssuedInsn(ctx, ctx.dispatch.outInsnType)
}

case class IssuedInsn(
    ctx: MachineContext,
    parentContextType: () => Bundle with DerefToInsn
) extends Bundle
    with DerefToInsn {
  val parentContext = parentContextType()
  val srcRegData = Vec(ctx.cfg.dataType, ctx.cfg.maxNumSrcRegsPerInsn)
}

case class Issue(ctx: MachineContext) extends Area {
  case class IqDependency() extends Bundle {
    val valid = Bool()
    val physRegIndex = ctx.cfg.physRegIndexType
  }

  object IqDependency {
    def idle: IqDependency = {
      val dep = IqDependency()
      dep.valid := False
      dep.physRegIndex.assignDontCare()
      dep
    }
  }

  case class IqTag() extends Bundle {
    val valid = Bool()
    val dependencies = Vec(IqDependency(), ctx.cfg.maxNumSrcRegsPerInsn)

    def depsReady: Bool = {
      dependencies
        .map(x => !x.valid || ctx.prfState.table(x.physRegIndex).dataAvailable)
        .andR
    }
  }

  object IqTag {
    def idle: IqTag = {
      val tag = IqTag()
      tag.valid := False
      for (i <- 0 until ctx.cfg.maxNumSrcRegsPerInsn) {
        tag.dependencies(i) := IqDependency.idle
      }
      tag
    }
  }

  case class IqData() extends Bundle {
    val insn = DispatchedInsn(ctx, ctx.dispatch.outInsnType)
  }

  val io = new Bundle {
    val issuePorts = Vec(Stream(IssuePort(ctx)), ctx.sem.functionUnits.size)
  }

  val iqTagSpace = Vec(Reg(IqTag()) init (IqTag.idle), ctx.cfg.issueQueueSize)
  val iqDataSpace = Mem(IqData(), ctx.cfg.issueQueueSize)

  val issuePushLogic = new Area {

    val dispatchOutput = ctx.dispatch.io.insnOutput.payload
    val renamedInsn = dispatchOutput.chainLookup[RenamedInsn]()

    // Generate new IQ tag + data
    val newIqTag = IqTag()
    val newIqData = IqData()

    newIqTag.valid := True
    newIqTag.dependencies := Vec(
      renamedInsn.insn.archSrcRegs
        .map(_.valid)
        .zip(renamedInsn.physSrcRegs)
        .map(arg => {
          val (valid, physRegIndex) = arg
          val out = IqDependency()
          out.valid := valid
          out.physRegIndex := physRegIndex
          out
        })
    )

    newIqData.insn := dispatchOutput

    val iqFreeMask = SetFromFirstOne(Vec(iqTagSpace.map(!_.valid)))
    val notFull = iqFreeMask.orR

    val iqDataAddr = UInt(log2Up(ctx.cfg.issueQueueSize) bits)
    iqDataAddr.assignDontCare()

    for (i <- 0 until ctx.cfg.issueQueueSize) {
      val first = i == 0
      val cond =
        if (first) iqFreeMask(i) else (iqFreeMask(i) =/= iqFreeMask(i - 1))
      when(cond) {
        assert(!iqTagSpace(i).valid)
        assert(notFull)
        iqDataAddr := i
        when(ctx.dispatch.io.insnOutput.valid) {
          iqTagSpace(i) := newIqTag
        }
      }
    }

    iqDataSpace.write(
      address = iqDataAddr,
      data = newIqData,
      enable = ctx.dispatch.io.insnOutput.fire
    )
    ctx.dispatch.io.insnOutput.ready := notFull
  }

  val issuePopLogic = new Area {
    val rrIndex = Reg(
      UInt(log2Up(ctx.cfg.issueQueueSize) bits)
    ) // round-robin index
    val rrFirstTag = iqTagSpace.zipWithIndex
      .map(arg =>
        (
          arg._1,
          U(arg._2, log2Up(ctx.cfg.issueQueueSize) bits),
          arg._1.depsReady
        )
      )
      .reduceBalancedTree((l, r) => {
        val outputTag = IqTag()
        val outputIndex = UInt(log2Up(ctx.cfg.issueQueueSize) bits)
        val outputDepsReady = Bool()
        when(rrIndex <= l._2 && l._1.valid && l._3) {
          outputTag := l._1
          outputIndex := l._2
          outputDepsReady := True
        } elsewhen (rrIndex <= r._2 && r._1.valid && r._3) {
          outputTag := r._1
          outputIndex := r._2
          outputDepsReady := True
        } otherwise {
          outputTag := IqTag.idle
          outputIndex.assignDontCare()
          outputDepsReady := False
        }
        (outputTag, outputIndex, outputDepsReady)
      })
    val seqFirstTag = iqTagSpace.zipWithIndex
      .map(arg =>
        (
          arg._1,
          U(arg._2, log2Up(ctx.cfg.issueQueueSize) bits),
          arg._1.depsReady
        )
      )
      .reduceBalancedTree((l, r) => {
        val outputTag = IqTag()
        val outputIndex = UInt(log2Up(ctx.cfg.issueQueueSize) bits)
        val outputDepsReady = Bool()
        when(l._1.valid && l._3) {
          outputTag := l._1
          outputIndex := l._2
          outputDepsReady := l._3
        } otherwise {
          outputTag := r._1
          outputIndex := r._2
          outputDepsReady := r._3
        }
        (outputTag, outputIndex, outputDepsReady)
      })
    val firstTag = IqTag()
    val firstIndex = UInt(log2Up(ctx.cfg.issueQueueSize) bits)
    when(rrFirstTag._1.valid) {
      firstTag := rrFirstTag._1
      firstIndex := rrFirstTag._2
    } otherwise {
      firstTag := seqFirstTag._1
      firstIndex := seqFirstTag._2
    }

    val iqContent =
      iqDataSpace.readAsync(address = firstIndex, readUnderWrite = writeFirst)
    val renamedInsn = iqContent.insn.chainLookup[RenamedInsn]()

    val srcRegContent =
      renamedInsn.physSrcRegs.map(x => ctx.prfContent.readAsync(x))

    val unifiedIssuePort = Stream(IssuePort(ctx))
    unifiedIssuePort.setBlocked()
    unifiedIssuePort.valid := firstTag.valid
    unifiedIssuePort.payload.insn.srcRegData := Vec(
      srcRegContent.map(x => x.data)
    )
    unifiedIssuePort.payload.insn.parentContext := iqContent.insn

    val issueOk = Vec(Bool(), io.issuePorts.size)
    issueOk.asBits := 0

    for ((fu, fuIndex) <- io.issuePorts.zipWithIndex) {
      when(
        iqContent.insn.insn.functionUnitTag === ctx.sem
          .functionUnits(fuIndex)
          .tag
      ) {
        fu << unifiedIssuePort
        issueOk(fuIndex) := True
      }
    }

    when(unifiedIssuePort.valid) {
      var issueCount = U(0, 2 bits)
      for(b <- issueOk) {
        val inc = b && issueCount =/= issueCount.maxValue
        issueCount = inc.mux(
          True -> (issueCount + 1),
          False -> issueCount
        )
      }
      assert(issueCount === 1)
    }

    when(unifiedIssuePort.fire) {
      assert(iqTagSpace(firstIndex).valid)
      iqTagSpace(firstIndex).valid := False
      rrIndex := firstIndex + 1
    }
  }
}
