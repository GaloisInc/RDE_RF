package Report


final case class LanguageFormatting(languageName: String,
                                    keywords: Array[String],
                                    lineComment: String,
                                    blockComment: BlockComment,
                                    literates: Array[Literate]) {
  def getLiterates: String = {
    if (literates.isEmpty) ""
    else literates.map(_.toString).mkString("| literate =", " ", ",")
  }
}
