package Parsers.ParserTypes

import Parsers.ParserTypes.ParsedReference
import Types.{ReferenceType, DocumentType}

case class ParsedRef(name: String,
                     originalLine: String,
                     documentName: String,
                     documentType: DocumentType.Value,
                     referenceType: ReferenceType.referenceType,
                     acronym: String)
  extends ParsedReference


