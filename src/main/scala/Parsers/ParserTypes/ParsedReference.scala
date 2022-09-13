package Parsers.ParserTypes

import Types.{ReferenceType, DocumentType}

trait ParsedReference(name: String,
                      originalLine: String,
                      referenceType: ReferenceType,
                      documentName: String,
                      documentType: DocumentType){
  require(originalLine.nonEmpty, "originalLine must not be empty")
  require(documentName.nonEmpty, "documentName must not be empty")
}
