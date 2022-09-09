package Formatter

import Types.DocReference.DocReference
import scala.collection.mutable

class InlineFormatter extends LatexFormatter {
  override def formatLatex(line: String): String = {
    require(line.nonEmpty, "line must not be empty")
    line
  } ensuring (_.eq(line), "InlineFormatter should not modify the input")
}

