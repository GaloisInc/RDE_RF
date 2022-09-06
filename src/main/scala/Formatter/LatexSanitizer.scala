package Formatter

import scala.util.matching.Regex

object LatexSanitizer {
  //Reference Strings in Latex cannot contain arbitrary characters.
  def sanitizeReferenceName(line: String): String = {
    require(line.nonEmpty, "Cannot sanitize empty string")
    line.replaceAll(" ", "_")
      .replaceAll("-", "_")
      .filterNot(_.equals("&"))
      .filterNot(_.equals("\\"))
  } ensuring ((l: String) => !l.contains(" ") && l.length <= line.length)

  //Reference Strings in Latex cannot contain arbitrary characters.
  def sanitizeName(line: String): String = {
    sanitizeWebLink(line)
  }

  def sanitizeWebLink(url: String): String = {
    url.replaceAll("_", Regex quoteReplacement """\_""")
  }
}
