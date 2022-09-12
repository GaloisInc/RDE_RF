package Formatter

import Types.DocReference.DocReference
import Types.LatexReferenceType

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
    latexInsideListing(webLink)
  } ensuring ((formatted: String) => formatted.startsWith(escapeListingBeginning)
    && formatted.endsWith(escapeListingEnding)
    && formatted.contains(url)
    && formatted.length > url.length)

  def addReference(reference: DocReference, currentDocument: String, referenceType: LatexReferenceType): String = {
    val hyperref = LatexSyntax.addReferenceInLatex(reference, currentDocument, referenceType)
    latexInsideListing(hyperref)
  } ensuring ((l: String) => l.contains(reference.getLabelText))

  def addAbstractions(abstractions: Set[DocReference], currentDocument: String): String = {
    val abstractionLinks = abstractions.map(ref => LatexSyntax.addReferenceInLatex(ref, currentDocument, LatexReferenceType.Abstraction))
    val abstractionText = abstractionLinks.mkString(s"($abstractionSymbolLatex", ", ", ")")
    formatLatexListing(abstractionText)
  }

  def addSpecializations(specializations: Set[DocReference], currentDocument: String): String = {
    val specializationLinks = specializations.map(ref => LatexSyntax.addReferenceInLatex(ref, currentDocument, LatexReferenceType.Refinement))
    val specializationText = specializationLinks.mkString(s"($refinementSymbolLatex", ", ", ")")
    formatLatexListing(specializationText)
  }

  def enrichLineWithLabel(originalLine: String, referenceText: String): String = {
    val cleanedLine = referenceText.trim()
    val label = LatexSyntax.addLabel(cleanedLine)
    val enrichedLine = originalLine + latexInsideListing(label)
    enrichedLine
  } ensuring ((enriched: String) => enriched.startsWith(originalLine) && enriched.contains(referenceText))

  def highlightLineWithReferences(originalLine: String, referencesToLinkTo: Option[Set[DocReference]]): String = {
    if (referencesToLinkTo.isEmpty) {
      originalLine
    } else {
      val references = referencesToLinkTo.get
      val referenceNameToLabelText = references.map(ref => ref.getName -> latexInsideListing(LatexSyntax.addClickableLocalLink(ref.getLabelText, ref.getName, LatexReferenceType.ConnectionArtifact)))
      val enrichedLine = referenceNameToLabelText.foldLeft(originalLine)((line, reference) => line.replace(reference._1, reference._2))
      enrichedLine
    }
  }

  protected def latexInsideListing(latexListing: String): String = {
    escapeListingBeginning + latexListing + escapeListingEnding
  }

  protected def formatLatexListing(text: String): String = {
    require(text.nonEmpty, "The string to format cannot be empty.")
    val formattedLatex = styleFormatter.formatLatex(text).trim.replaceAll("\\s+", " ")
    latexInsideListing(formattedLatex)
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
