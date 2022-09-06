package Formatter

class MarginFomatter extends LatexFormatter {
  override def formatLatex(line: String): String = {
    require(line.nonEmpty)
    s"\\marginnote{$line}"
  }

}
