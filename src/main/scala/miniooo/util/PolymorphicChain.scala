package miniooo.util

import spinal.core._
import spinal.lib._
import scala.reflect._

trait PolymorphicDataChain extends Data {
  def parentObjects: Seq[_ <: Data]
  def decodeAs[T <: AnyRef](ctag: ClassTag[T]): Option[T] = None

  private def doLookup[T <: AnyRef](ctag: ClassTag[T]): T = {
    try {
      return ctag.runtimeClass.cast(this).asInstanceOf[T]
    } catch {
      case _: ClassCastException => {}
    }

    val decoded = this.decodeAs(ctag)
    if (decoded.isDefined) {
      return decoded.get
    }

    if (this.parentObjects != null) {
      for (obj <- this.parentObjects) {
        if (obj != null) {
          try {
            val x = obj.asInstanceOf[PolymorphicDataChain].doLookup[T](ctag)
            if (x != null) return x
          } catch {
            case _: ClassCastException => {}
          }
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

  def tryLookup[T <: AnyRef: ClassTag]: Option[T] = {
    val x = doLookup[T](classTag[T])
    if(x == null) None else Some(x)
  }
}
