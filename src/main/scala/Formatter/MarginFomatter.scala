package Formatter

class MarginFomatter extends LatexFormatter {
  override def formatLatex(line: String): String = {
    require(line.nonEmpty)
    s"\\marginnote{$line}"
  } ensuring ((res: String) => res.nonEmpty && res.startsWith("\\marginnote{") && res.endsWith("}") && res.contains(line))

}
