package Formatter

import Formatter.LatexSanitizer.{sanitizeName, sanitizeWebLink}
import Types.DocReference

object LatexSyntax {
  def addLabel(reference: String): String = s"\\label{$reference}" // \\hypertarget{$reference}{}"

  def addClickableLocalLink(reference: String, nameOfReference: String): String = {
    s"\\hyperref[$reference]{${sanitizeName(nameOfReference)}}"
  }

  def addCref(reference: String): String = s"\\cref{$reference}"

  def addVref(reference: String): String = s"\\vref{$reference}"

  def addReference(reference: DocReference, currentDocument: String): String = {
    val hyperref = addReferenceInLatex(reference, currentDocument)
    hyperref
  } ensuring ((l: String) => l.contains("\\hyperref") && l.contains(reference.getLabelText))


  def addReferenceInLatex(reference: DocReference, currentDocument: String): String = {
    s"${addClickableLocalLink(reference.getLabelText, reference.getName)} (${explicitReference(reference, currentDocument)})"
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
}
