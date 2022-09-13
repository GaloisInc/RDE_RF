package Parsers.ParserTypes

import Parsers.ParserTypes.ParsedReference
import Types.{ReferenceType, DocumentType}

case class ParsedRef(name: String,
                     originalLine: String,
                     documentName: String,
                     documentType: DocumentType,
                     referenceType: ReferenceType,
                     acronym: String)
  extends ParsedReference(name, originalLine, referenceType, documentName, documentType)


