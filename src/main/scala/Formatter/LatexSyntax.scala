package Formatter

import Formatter.LatexSanitizer.{sanitizeName, sanitizeWebLink}
import Types.DocReference.DocReference
import Types.{DocumentType, LatexReferenceTypes}

object LatexSyntax {
  def addLabel(reference: String): String = {
    require(reference.nonEmpty, "Reference must not be empty")
    require(reference.matches("^[a-zA-Z0-9_]*$"), "Reference must only contain alphanumeric characters and underscores")
    s"""\\label{$reference}"""
  } ensuring((result: String) => result.nonEmpty && result.contains(reference), "Result must contain reference")
  //\\hypertarget{$reference}{}"

  def addClickableLocalLink(reference: String, nameOfReference: String, referenceType: LatexReferenceTypes.Value): String = {
    require(nameOfReference.nonEmpty, "nameOfReference must not be empty")
    val sanitizedReference = sanitizeName(nameOfReference)
    referenceType match {
      case LatexReferenceTypes.File => s"\\href{run./$reference}{$sanitizedReference}"
      case LatexReferenceTypes.Abstraction => s"\\abstractionLink{$reference}{$sanitizedReference}"
      case LatexReferenceTypes.Refinement => s"\\refinementLink{$reference}{$sanitizedReference}"
      case LatexReferenceTypes.Link => s"\\link{$reference}{$sanitizedReference}"
      case LatexReferenceTypes.CryptolProperty => s"\\script{$reference}{$sanitizedReference}"
      case LatexReferenceTypes.ConnectionArtifact => s"\\hyperref[$reference]{$sanitizedReference}"
    }
  } ensuring((result: String) => result.nonEmpty && result.contains(reference), "Result must contain reference.")


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
    require(reference.matches("^[a-zA-Z0-9_]*$"), "Reference must only contain alphanumeric characters and underscores")
    s"\\cref{$reference}"
  } ensuring((line: String) => line.contains(reference) && line.contains("cref"), "Reference must be contained in cref")

  def addVref(reference: String): String = {
    require(reference.nonEmpty, "Reference must not be empty")
    require(reference.matches("^[a-zA-Z0-9_]*$"), "Reference must only contain alphanumeric characters and underscores")
    s"\\vref{$reference}"
  } ensuring((line: String) => line.contains(reference) && line.contains("vref"), "Reference must be contained in vref")

  def addReferenceInLatex(reference: DocReference, currentDocument: String, referenceType: LatexReferenceTypes.latexReferenceType): String = {
    s"${addClickableLocalLink(reference.getLabelText, reference.getShortName, referenceType)} (${explicitReference(reference, currentDocument)})"
  }

  private def explicitReference(ref: DocReference, documentName: String): String = {
    if (ref.documentName.equalsIgnoreCase(documentName)) addCref(ref.getLabelText)
    else addVref(ref.getLabelText)
  } ensuring ((l: String) => l.contains(ref.getLabelText))


  def createWebLink(url: String): String = {
    require(url.nonEmpty, "URL must not be empty")
    require(url.startsWith("http"), "URL must start with http")
    val sanitizedUrl = sanitizeWebLink(url)
    s"\\href{$url}{$sanitizedUrl}"
  } ensuring ((formatted: String) =>
    formatted.contains(url)
      && formatted.length > url.length)


  def generateSection(sectionName: String): String = {
    s"""\\section{${LatexSanitizer.sanitizeName(sectionName)}}
       |\\label{sec:${LatexSanitizer.sanitizeReferenceName(sectionName)}}""".stripMargin
  }

  def generateSubSection(name: String): String = {
    require(name.nonEmpty, "Name must not be empty")
    s"""\\subsection{${LatexSanitizer.sanitizeName(name)}}
       |\\label{subsec:${LatexSanitizer.sanitizeReferenceName(name)}}""".stripMargin
  }

  def generateSubSubSection(name: String, labelText: String): String = {
    require(name.nonEmpty, "Name must not be empty")
    require(labelText.nonEmpty, "Label text must not be empty")
    s"""\\subsubsection{$name}
       |\\label{subsubsec:${LatexSanitizer.sanitizeReferenceName(labelText)}}""".stripMargin
  }

  def beginDocument(title: String): String = {
    require(title.nonEmpty, "Title must not be empty")
    """|\begin{document}
       |\maketitle
       |\tableofcontents
       |""".stripMargin
  }

  val endDocument: String = "\\end{document}"
}