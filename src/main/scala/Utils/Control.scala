package Utils

import Types.*

import java.nio.file.{Files, Paths}
import scala.language.reflectiveCalls

object Control {
  def using[A <: {def close(): Unit}, B](resource: A)(f: A => B): B =
    try f(resource) finally resource.close()
}


