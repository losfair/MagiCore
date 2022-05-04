package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._
import spinal.core.fiber._
import scala.collection.mutable.ArrayBuffer
import spinal.lib.misc.plic.PlicGateway
import spinal.lib.misc.plic.PlicMapping
import spinal.lib.generator._
import spinal.lib.misc.plic.PlicGatewayActiveHigh
import spinal.lib.misc.plic.PlicTarget
import spinal.lib.misc.plic.PlicMapper

case class Axi4PlicGenerator(axiConfig: Axi4Config) extends Area with InterruptCtrlGeneratorI{
  @dontName val gateways = ArrayBuffer[Handle[PlicGateway]]()
  val ctrl = Handle(logic.axi4)

  val priorityWidth = Handle[Int]
  val mapping = Handle[PlicMapping]

  val lock = Lock()

  case class TargetModel(target : Handle[Bool], clockDomain : Handle[ClockDomain])
  val targetsModel = ArrayBuffer[TargetModel]()
  def addTarget(target : Handle[Bool]) = {
    val id = targetsModel.size
    targetsModel += TargetModel(target, ClockDomain.currentHandle)

    //TODO remove the need of delaying stuff for name capture
    Handle(Component.current.addTag(new Export(Axi4PlicGenerator.this.getName() + "_" + target.getName, id)))
  }

  override def addInterrupt(source : => Handle[Bool], id : Int) = {
    lock.retain()
    Handle{
      val src = source
      soon(lock)
      gateways += PlicGatewayActiveHigh(
        source = src,
        id = id,
        priorityWidth = priorityWidth
      ).setCompositeName(src, "plic_gateway")

      Component.current.addTag (new Export(Axi4PlicGenerator.this.getName() + "_" + src.getName, id))
      lock.release()
    }
  }

  override def getBus(): Handle[Nameable] = ctrl

  val logic = Handle(new Area{
    lock.await()
    val axi4 = Axi4(axiConfig)
    val bus = new Axi4SlaveFactory(axi4)
    val targets = targetsModel.map(flag =>
      PlicTarget(
        gateways = gateways.map(_.get),
        priorityWidth = priorityWidth
      ).setCompositeName(flag.target, "plic_target")
    )

    //    gateways.foreach(_.priority := 1)
    //    targets.foreach(_.threshold := 0)
    //    targets.foreach(_.ie.foreach(_ := True))

    val bridge = PlicMapper(bus, mapping)(
      gateways = gateways.map(_.get),
      targets = targets
    )

    for(targetId <- 0 until targetsModel.length){
      def bufferize[T <: Data](that : T) : T = if(targetsModel(targetId).clockDomain != ClockDomain.currentHandle) targetsModel(targetId).clockDomain on BufferCC[T](that, init = null.asInstanceOf[T]) else RegNext[T](that)
      targetsModel(targetId).target := bufferize(targets(targetId).iep)
    }
  })
}