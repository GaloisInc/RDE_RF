package Utils

import Types.*

import java.nio.file.{Files, Paths}
import scala.language.reflectiveCalls
import DocReference.DocReference

object Control {
  def using[A <: {def close(): Unit}, B](resource: A)(f: A => B): B =
    try f(resource) finally resource.close()

  def extractReferences(filePath: String, transformReference: (String) => Option[DocReference]): Set[DocReference] = {
    using(io.Source.fromFile(filePath)) { source => {
      val lines = source.getLines().toArray
      lines.map(line => {
        transformReference(line)
      }).filter(_.isDefined).map(_.get)
        .toSet
    }
    }
  }
}


