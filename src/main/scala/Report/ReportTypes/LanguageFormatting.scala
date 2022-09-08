package Report.ReportTypes

final case class LanguageFormatting(languageName: String,
                                    keywords: Array[String],
                                    lineComment: String,
                                    blockComment: BlockComment,
                                    literates: Array[Literate]) {

  require(languageName.nonEmpty, "Language name cannot be empty")
  require(keywords.nonEmpty, "Keywords cannot be empty")
  require(lineComment.nonEmpty, "Line comment cannot be empty")

  def getLiterates: String = {
    if (literates.isEmpty) ""
    else literates.map(_.toString).mkString("| literate =", " ", ",")
  }
}
