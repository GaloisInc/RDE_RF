package DocumentEnrichers

import Types.DocReference

import scala.collection.mutable


class LatexFormatter {
  def addReference(reference: DocReference, currentDocument: String): String = {
    val href = addReferenceInLatex(reference, currentDocument)
    formatLatexListing(href)
  } ensuring ((l: String) => l.contains("\\href") && l.contains(reference.referenceName.reference))

  def referenceCref(ref: DocReference, documentName: String): String = {
    val text = new mutable.StringBuilder(addCref(ref.referenceName.reference))
    val pageRef = if !documentName.equals(ref.documentName) then s" on ${addPageRef(ref.referenceName.reference)}" else ""
    text ++= pageRef
    text.toString()
  } ensuring ((l: String) => l.contains("\\cref") && l.contains(ref.referenceName.reference))

  private def addReferenceInLatex(reference: DocReference, currentDocument: String): String = {
    s"${addHRef(reference.referenceName.reference, reference.referenceName.name)} (${referenceCref(reference, currentDocument)})"
  }

  private def formatLatexListing(text: String): String = {
    require(text.nonEmpty)
    s"(*$text*)"
  } ensuring ((encodedText: String) => encodedText.startsWith("(*") && encodedText.endsWith("*)") && encodedText.contains(text))

  private def addLabel(reference: String): String = s"\\label{$reference}"

  private def addHRef(reference: String, name: String): String = s"\\href{$reference}{$name}"

  private def addPageRef(reference: String): String = s"\\cpageref{$reference}"

  private def addCref(reference: String): String = s"\\cref{$reference}"

  def enrichLineWithLabel(originalLine: String, referenceText: String): String = {
    val label = addLabel(referenceText)
    val enrichedLine = originalLine + formatLatexListing(label)
    enrichedLine
  } ensuring ((enriched: String) => enriched.startsWith(originalLine) && enriched.contains(referenceText))

  def sanitizeLine(line: String): String = {
    line.replaceAll(" ", "_")
      .replaceAll("-", "_")
      .filterNot(_.equals("&"))
      .filterNot(_.equals("\\"))
  } ensuring ((l: String) => !l.contains(" "))

  def referenceText(fileName: String, line: String): String = {
    require(fileName.nonEmpty && line.nonEmpty)
    val sanitizedLine = sanitizeLine(line)
    s"${fileName}_$sanitizedLine"
  } ensuring ((referenceText: String) => referenceText.contains(fileName) && referenceText.contains(line))

  def addAbstractions(abstractions: Set[DocReference], currentDocument: String): String = {
    val abstractionLinks = abstractions.map(ref => addReferenceInLatex(ref, currentDocument))
    val abstractionText = abstractionLinks.mkString("(specializes: ", ", ", ")")
    formatLatexListing(abstractionText)
  }

  def addSpecialization(specializations: Set[DocReference], currentDocument: String): String = {
    val specializationLinks = specializations.map(ref => addReferenceInLatex(ref, currentDocument))
    val specializationText = specializationLinks.mkString("(abstracts: ", ", ", ")")
    formatLatexListing(specializationText)
  }
}