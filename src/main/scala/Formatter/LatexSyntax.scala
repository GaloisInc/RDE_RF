package Formatter

import Formatter.LatexSanitizer.{sanitizeName, sanitizeWebLink}
import Types.DocReference.DocReference
import Types.{DocumentType, LatexReferenceTypes}

object LatexSyntax {
  def addLabel(reference: String): String = s"\\label{$reference}" // \\hypertarget{$reference}{}"

  def addClickableLocalLink(reference: String, nameOfReference: String, referenceType: LatexReferenceTypes.latexReferenceType): String = {
    val sanitizedReference = sanitizeName(nameOfReference)
    referenceType match {
      case LatexReferenceTypes.File => s"\\href{run./$reference}{$sanitizedReference}"
      case LatexReferenceTypes.Abstraction => s"\\abstractionLink{$reference}{$sanitizedReference}"
      case LatexReferenceTypes.Refinement => s"\\refinementLink{$reference}{$sanitizedReference}"
      case LatexReferenceTypes.Link => s"\\link{$reference}{$sanitizedReference}"
      case LatexReferenceTypes.CryptolProperty => s"\\script{$reference}{$sanitizedReference}"
      case LatexReferenceTypes.ConnectionArtifact => s"\\hyperref[$reference]{$sanitizedReference}"
    }
  }


  def colorText(text: String, documentType: DocumentType.documentType): String = {
    require(text.nonEmpty, "Text must not be empty")
    val color = documentType match {
      case DocumentType.Lando => "blue"
      case DocumentType.Lobot => "red"
      case DocumentType.SysML => "green"
      case DocumentType.Cryptol => "purple"
      case DocumentType.Saw => "orange"
      case DocumentType.SV => "brown"
      case DocumentType.BSV => "pink"
    }
    s"\\textcolor{$color}{$text}"
  } ensuring(coloredString => coloredString.contains(text), "Text must be contained in colored string")

  def addCref(reference: String): String = {
    require(reference.nonEmpty, "Reference must not be empty")
    s"\\cref{$reference}"
  } ensuring (_.contains(reference))

  def addVref(reference: String): String = {
    require(reference.nonEmpty, "Reference must not be empty")
    s"\\vref{$reference}"
  } ensuring (_.contains(reference))

  def addReferenceInLatex(reference: DocReference, currentDocument: String, referenceType: LatexReferenceTypes.latexReferenceType): String = {
    s"${addClickableLocalLink(reference.getLabelText, reference.getShortName, referenceType)} (${explicitReference(reference, currentDocument)})"
  }

  private def explicitReference(ref: DocReference, documentName: String): String = {
    if (ref.documentName.equalsIgnoreCase(documentName)) addCref(ref.getLabelText)
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