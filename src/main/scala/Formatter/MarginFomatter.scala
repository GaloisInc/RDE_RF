package Formatter

class MarginFomatter extends LatexFormatter {
  val fontSize = "tiny"
  override def formatLatex(line: String): String = {
    require(line.nonEmpty)
    val formattedString =
      s"""\\marginpar{
         |\\begin{$fontSize}
         |${line}
         |\\end{$fontSize}
         |\\par
         |}""".stripMargin
    formattedString
  } ensuring ((res: String) => res.nonEmpty && res.startsWith("\\marginpar{") && res.endsWith("}") && res.contains(line))

}
