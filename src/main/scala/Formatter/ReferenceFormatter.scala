package Formatter
import Report.{ClickableLink, Label, LatexElement, LatexReference, Weblink}
import Types.DocReference.DocReference
import Types.LatexReferenceTypes

class ReferenceFormatter(
                          styleFormatter: LatexFormatter,
                          private val escapeListingBeginning: String = "(@",
                          private val escapeListingEnding: String = "@)",
                          protected val abstractionSymbolLatex: String = "$\\sqsupseteq$",
                          protected val refinementSymbolLatex: String = "$\\sqsubseteq$") {

  def createWebLink(url: String): Weblink = {
    require(url.nonEmpty, "url must not be empty")
    require(url.startsWith("http"), "url must start with http")
    Weblink(url, url, formatInsideListing)
  } ensuring ((formatted: Weblink) => formatted.toLatex.startsWith(escapeListingBeginning)
    && formatted.toLatex.endsWith(escapeListingEnding)
    && formatted.toLatex.contains(url)
    && formatted.toLatex.length > url.length)

  def addReference(reference: DocReference, currentDocument: String, referenceType: LatexReferenceTypes.Value): String = {
    require(currentDocument.nonEmpty, "nameOfDocument must not be empty")
    val hyperref = addReferenceInLatex(reference, currentDocument, referenceType, formatInsideListing)
    hyperref.map(_.toLatex).mkString(" ")
  } ensuring ((l: String) => l.contains(reference.getLabelText))

  def addAbstractions(abstractions: Set[DocReference], nameOfDocument: String): String = {
    require(nameOfDocument.nonEmpty, "nameOfDocument must not be empty")
    val abstractionLinks = abstractions.map(ref => addReferenceInLatex(ref, nameOfDocument, LatexReferenceTypes.Abstraction, identity).map(_.toLatex).mkString(" "))
    val abstractionText = abstractionLinks.mkString(s"($abstractionSymbolLatex", ", ", ")")
    formatLatexListing(abstractionText)
  }

  def addSpecializations(specializations: Set[DocReference], nameOfDocumet: String): String = {
    require(nameOfDocumet.nonEmpty, "nameOfDocument must not be empty")
    val specializationLinks = specializations.map(ref => addReferenceInLatex(ref, nameOfDocumet, LatexReferenceTypes.Refinement, identity).map(_.toLatex).mkString(" "))
    val specializationText = specializationLinks.mkString(s"($refinementSymbolLatex", ", ", ")")
    formatLatexListing(specializationText)
  }

  def enrichLineWithLabel(originalLine: String, referenceText: String): String = {
    val cleanedLine = referenceText.trim()
    val label = Label(cleanedLine, formatInsideListing)
    val enrichedLine = originalLine + label.toLatex
    enrichedLine
  } ensuring ((enriched: String) => enriched.startsWith(originalLine) && enriched.contains(referenceText))

  def highlightLineWithReferences(originalLine: String,
                                  symbol: String,
                                  referencesToLinkTo: Map[String, DocReference]): String = {
    if (referencesToLinkTo.isEmpty) originalLine
    else {
      val firstPart = originalLine.split(symbol).head
      val secondPart = originalLine.split(symbol).last
      val enrichedSecondPart =
        referencesToLinkTo
          .toList
          .sortBy(_._1.length)
          .foldLeft(secondPart)((line, ref) => {
            val label = ClickableLink(ref._2.getLabelText, ref._2.shortName, LatexReferenceTypes.ConnectionArtifact, formatInsideListing).toLatex
            line.replace(ref._1, label)
          })
      val resultString = firstPart + " " + symbol + " " + enrichedSecondPart
      resultString
    }
  }


  /**
   * Formats a string to be used inside a latex listing.
   *
   * @param contant the string to format
   * @return the formatted string
   */
  private def formatInsideListing(contant: String): String = {
    require(contant.nonEmpty, "latexListing must not be empty")
    escapeListingBeginning + contant + escapeListingEnding
  } ensuring ((formatted: String) => formatted.startsWith(escapeListingBeginning)
    && formatted.endsWith(escapeListingEnding)
    && formatted.contains(contant)
    && formatted.length > contant.length)

  private def formatLatexListing(text: String): String = {
    require(text.nonEmpty, "The string to format cannot be empty.")
    val formattedLatex = styleFormatter.formatLatex(text).trim.replaceAll("\\s+", " ")
    formatInsideListing(formattedLatex)
  } ensuring ((encodedText: String) =>
    encodedText.startsWith(escapeListingBeginning)
      && encodedText.endsWith(escapeListingEnding)
      && encodedText.contains(text))


  private def addReferenceInLatex(reference: DocReference,
                                  currentDocument: String,
                                  referenceType: LatexReferenceTypes.latexReferenceType,
                                  formatting: String => String
                                 ): List[LatexElement] = {
    require(currentDocument.nonEmpty, "nameOfDocument must not be empty")

    val link = ClickableLink(reference.getLabelText, reference.shortName, referenceType, formatting)
    val formattedReference = if (reference.documentName.equals(currentDocument)) {
      LatexReference(reference.getLabelText, "cref", formatting)
    } else {
      LatexReference(reference.getLabelText, "vref", formatting)
    }
    List(link, formattedReference)
  }

  def referenceCrefs(references: Set[DocReference], documentName: String): LatexReference = {
    val referencesNames: String = references.map(_.getLabelText).mkString(",")
    if (references.forall(ref => ref.documentName.equals(documentName))) LatexReference(referencesNames, "cref", formatInsideListing)
    else LatexReference(referencesNames, "vref", formatInsideListing)
  } ensuring ((l: LatexReference) => references.forall(ref => l.toLatex.contains(ref.getLabelText)))


}
