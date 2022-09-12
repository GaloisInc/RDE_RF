package Formatter

import Formatter.LatexSanitizer.{sanitizeName, sanitizeWebLink}
import Types.DocReference.DocReference
import Types.LatexReferenceType

object LatexSyntax {
  def addLabel(reference: String): String = s"\\label{$reference}" // \\hypertarget{$reference}{}"

  def addClickableLocalLink(reference: String, nameOfReference: String, referenceType: LatexReferenceType): String = {
    val sanitizedReference = sanitizeName(nameOfReference)
    referenceType match
      case LatexReferenceType.File => s"\\href{run./$reference}{$sanitizedReference}"
      case LatexReferenceType.Abstraction => s"\\abstractionLink{$reference}{$sanitizedReference}"
      case LatexReferenceType.Refinement => s"\\refinementLink{$reference}{$sanitizedReference}"
      case LatexReferenceType.Link => s"\\link{$reference}{$sanitizedReference}"
      case LatexReferenceType.CryptolProperty => s"\\script{$reference}{$sanitizedReference}"
      case LatexReferenceType.ConnectionArtifact => s"\\hyperref[$reference]{$sanitizedReference}"
  }


  def addCref(reference: String): String = {
    require(reference.nonEmpty, "Reference must not be empty")
    s"\\cref{$reference}"
  } ensuring (_.contains(reference))

  def addVref(reference: String): String = {
    require(reference.nonEmpty, "Reference must not be empty")
    s"\\vref{$reference}"
  } ensuring (_.contains(reference))

  def addReferenceInLatex(reference: DocReference, currentDocument: String, referenceType: LatexReferenceType): String = {
    s"${addClickableLocalLink(reference.getLabelText, reference.getShortName, referenceType)} (${explicitReference(reference, currentDocument)})"
  }

  private def explicitReference(ref: DocReference, documentName: String): String = {
    if ref.documentName.equals(documentName)
    then addCref(ref.getLabelText)
    else addVref(ref.getLabelText)
  } ensuring ((l: String) => l.contains(ref.getLabelText))


  def createWebLink(url: String): String = {
    require(url.nonEmpty)
    s"\\href{$url}{${sanitizeWebLink(url)}}"
  } ensuring ((formatted: String) =>
    formatted.contains(url)
      && formatted.length > url.length)


  def generateSection(sectionName: String): String = {
    s"""\\section{${LatexSanitizer.sanitizeName(sectionName)}}
       |\\label{sec:${LatexSanitizer.sanitizeReferenceName(sectionName)}}""".stripMargin
  }

  def generateSubSection(name: String): String = {
    s"""\\subsection{${LatexSanitizer.sanitizeName(name)}}
       |\\label{subsec:${LatexSanitizer.sanitizeReferenceName(name)}}""".stripMargin
  }

  def generateSubSubSection(name: String, labelText: String): String = {
    s"""\\subsubsection{$name}
       |\\label{subsubsec:${LatexSanitizer.sanitizeReferenceName(labelText)}}""".stripMargin
  }

  val beginDocument: String = "\\begin{document}"

  val endDocument: String = "\\end{document}"
}
