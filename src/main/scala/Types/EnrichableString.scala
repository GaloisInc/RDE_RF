package Types

abstract class EnrichableString {
  def documentName: String

  def originalLine: String

  def enrichedLine: Option[String]

  require(documentName.nonEmpty)
  require(originalLine.nonEmpty)
  //require(enrichedLine.isEmpty || (enrichedLine.isDefined && enrichedLine.get.contains(originalLine)))
}
