package Formatter

import Types.DocReference

abstract class Formatter {
  private val escapeListingBeginning: String = "(*"
  private val escapeListingEnding: String = "*)"

  def addReference(reference: DocReference, currentDocument: String): String

  def addAbstractions(abstractions: Set[DocReference], currentDocument: String): String

  def addSpecialization(specializations: Set[DocReference], currentDocument: String): String

  def createLink(url: String): String = {
    require(url.nonEmpty)
    val href = s"\\href{$url}{$url}"
    formatLatexListing(href)
  } ensuring ((formatted: String) => formatted.startsWith(escapeListingBeginning)
    && formatted.endsWith(escapeListingEnding)
    && formatted.contains(url)
    && formatted.length > url.length)

  def enrichLineWithLabel(originalLine: String, referenceText: String): String = {
    val cleanedLine = referenceText.trim()
    val label = addLabel(cleanedLine)
    val enrichedLine = originalLine + formatLatexListing(label)
    enrichedLine
  } ensuring ((enriched: String) => enriched.startsWith(originalLine) && enriched.contains(referenceText))

  private def addLabel(reference: String): String = s"\\label{$reference}"

  def sanitizeLine(line: String): String = {
    line.replaceAll(" ", "_")
      .replaceAll("-", "_")
      .filterNot(_.equals("&"))
      .filterNot(_.equals("\\"))
  } ensuring ((l: String) => !l.contains(" ") && l.length <= line.length)

  protected def formatLatexListing(text: String): String = {
    require(text.nonEmpty)
    escapeListingBeginning + text + escapeListingEnding
  } ensuring ((encodedText: String) =>
    encodedText.startsWith(escapeListingBeginning)
      && encodedText.endsWith(escapeListingEnding)
      && encodedText.contains(text))
}
