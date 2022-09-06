package Formatter

import Types.DocReference

import scala.collection.mutable

class InlineFormatter extends LatexFormatter {
  override def formatLatex(line: String): String = {
    require(line.nonEmpty)
    line
  }
}

