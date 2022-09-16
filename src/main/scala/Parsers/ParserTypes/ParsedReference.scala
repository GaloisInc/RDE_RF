package Parsers.ParserTypes

import Types.{ReferenceType, DocumentType}

trait ParsedReference{
  def name: String
  def originalLine: String

  def referenceType: ReferenceType.referenceType

  def documentName: String

  def documentType: DocumentType.documentType
  require(originalLine.nonEmpty, "originalLine must not be empty")
  require(documentName.nonEmpty, "documentName must not be empty")
}
