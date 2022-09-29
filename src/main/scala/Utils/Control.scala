package Utils

import Types.DocReference.DocReference

import scala.language.reflectiveCalls

object Control {
  def using[A <: {def close(): Unit}, B](resource: A)(f: A => B): B =
    try f(resource) finally resource.close()

  def extractReferences(filePath: String, transformReference: (String) => Option[DocReference]): Set[DocReference] = {
    using(io.Source.fromFile(filePath)(io.Codec.UTF8)) { source => {
      val lines = source.getLines().toArray
      lines.map(line => {
        transformReference(line)
      }).filter(_.isDefined).map(_.get)
        .toSet
    }
    }
  }
}


