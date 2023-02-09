package Types

import Formatter.ReferenceFormatter

trait DecorateableString {
  def enrich(formatter: ReferenceFormatter): String
  def originalLine: String

  require(originalLine.nonEmpty, "originalLine must not be empty")
}


