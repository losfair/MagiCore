package miniooo.util

import spinal.core._
import spinal.lib._
import scala.reflect._

trait PolymorphicDataChain extends Data {
  def parentObjects: Seq[_ <: Data]

  private def doLookup[T <: AnyRef](ctag: ClassTag[T]): T = {
    try {
      return ctag.runtimeClass.cast(this).asInstanceOf[T]
    } catch {
      case _: ClassCastException => {}
    }
    if (this.parentObjects != null) {
      for (obj <- this.parentObjects) {
        try {
          val x = obj.asInstanceOf[PolymorphicDataChain].doLookup[T](ctag)
          if (x != null) return x
        } catch {
          case _: ClassCastException => {}
        }
      }
    }
    return null.asInstanceOf[T]
  }

  def lookup[T <: AnyRef: ClassTag]: T = {
    val obj = doLookup[T](classTag[T])
    if (obj == null) {
      SpinalError("Cannot find object of type " + classTag[T].toString())
    }
    obj
  }
}
