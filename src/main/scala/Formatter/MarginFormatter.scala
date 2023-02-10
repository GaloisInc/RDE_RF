package Formatter

class MarginFormatter extends LatexFormatter {
  private val fontSize = "tiny"

  override def formatLatex(line: String): String = {
    require(line.nonEmpty, "line must not be empty")
    val formattedString =
      s"""\\marginpar{
         |\\begin{$fontSize}
         |$line
         |\\end{$fontSize}
         |\\par
         |}""".stripMargin
    formattedString
  } ensuring ((res: String) => res.nonEmpty && res.startsWith("\\marginpar{") && res.endsWith("}") && res.contains(line))
}
