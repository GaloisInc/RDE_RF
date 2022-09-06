package Formatter

import scala.util.matching.Regex

object LatexSanitizer {
  //Reference Strings in Latex cannot contain arbitrary characters.
  def sanitizeReferenceName(referenceName: String): String = {
    require(referenceName.nonEmpty, "Cannot sanitize empty string")
    referenceName.replaceAll(" ", "_")
      .replaceAll("-", "_")
      .replaceAll("""\\&""", "_")
  } ensuring ((l: String) => !l.contains(" ") && l.length <= referenceName.length)

  //Reference Strings in Latex cannot contain arbitrary characters.
  def sanitizeName(line: String): String = {
    sanitizeWebLink(line)
  }

  def sanitizeWebLink(url: String): String = {
    url.replaceAll("_", Regex quoteReplacement """\_""")
  }
}
