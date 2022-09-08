package Formatter

import Types.DocReference
import scala.util.matching.Regex

class ReferenceFormatter(
                 styleFormatter: LatexFormatter,
                 private val escapeListingBeginning: String = "(*",
                 private val escapeListingEnding: String = "*)",
                 protected val abstractionSymbolLatex: String = "$\\sqsupseteq$",
                 protected val refinementSymbolLatex: String = "$\\sqsubseteq$") {

  def createWebLink(url: String): String = {
    require(url.nonEmpty, "url must not be empty")
    val webLink = LatexSyntax.createWebLink(url)
    formatLatexListing(webLink)
  } ensuring ((formatted: String) => formatted.startsWith(escapeListingBeginning)
    && formatted.endsWith(escapeListingEnding)
    && formatted.contains(url)
    && formatted.length > url.length)

  def addReference(reference: DocReference, currentDocument: String): String = {
    val hyperref = LatexSyntax.addReferenceInLatex(reference, currentDocument)
    formatLatexListing(hyperref)
  } ensuring ((l: String) => l.contains("\\hyperref") && l.contains(reference.getLabelText))

  def addAbstractions(abstractions: Set[DocReference], currentDocument: String): String = {
    val abstractionLinks = abstractions.map(ref => LatexSyntax.addReferenceInLatex(ref, currentDocument))
    val abstractionText = abstractionLinks.mkString(s"($abstractionSymbolLatex", ", ", ")")
    formatLatexListing(abstractionText)
  }

  def addSpecializations(specializations: Set[DocReference], currentDocument: String): String = {
    val specializationLinks = specializations.map(ref => LatexSyntax.addReferenceInLatex(ref, currentDocument))
    val specializationText = specializationLinks.mkString(s"($refinementSymbolLatex", ", ", ")")
    formatLatexListing(specializationText)
  }

  def enrichLineWithLabel(originalLine: String, referenceText: String): String = {
    val cleanedLine = referenceText.trim()
    val label = LatexSyntax.addLabel(cleanedLine)
    val enrichedLine = originalLine + formatLatexListing(label)
    enrichedLine
  } ensuring ((enriched: String) => enriched.startsWith(originalLine) && enriched.contains(referenceText))


  protected def formatLatexListing(text: String): String = {
    require(text.nonEmpty, "The string to format cannot be empty.")
    val escapedText = escapeListingBeginning + text + escapeListingEnding
    styleFormatter.formatLatex(escapedText)
  } ensuring ((encodedText: String) =>
    encodedText.startsWith(escapeListingBeginning)
      && encodedText.endsWith(escapeListingEnding)
      && encodedText.contains(text))

  def referenceCrefs(references: Set[DocReference], documentName: String): String = {
    val referencesNames: String = references.map(_.getLabelText).mkString(",")
    if references.forall(ref => ref.documentName.equals(documentName))
    then LatexSyntax.addCref(referencesNames)
    else LatexSyntax.addVref(referencesNames)
  } ensuring ((l: String) => references.forall(ref => l.contains(ref.getLabelText)))



}
