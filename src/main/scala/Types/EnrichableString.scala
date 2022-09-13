package Types

import Formatter.ReferenceFormatter

trait EnrichableString {
  def enrichedLine(formatter: ReferenceFormatter): String

  def originalLine: String

  require(originalLine.nonEmpty, "originalLine must not be empty")
}


