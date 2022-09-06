package Types

import Formatter.ReferenceFormatter

abstract class EnrichableString {
  def documentName: String

  def originalLine: String
  def enrichedLine(formatter: ReferenceFormatter): String

  require(documentName.nonEmpty, "documentName must not be empty")
  require(originalLine.nonEmpty, "originalLine must not be empty")
}
