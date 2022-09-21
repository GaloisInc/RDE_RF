package Parsers.ParserTypes

import Types.{ReferenceType, DocumentType}

case class ParsedRefWithReferences(name: String,
                                   originalLine: String,
                                   documentName: String,
                                   documentType: DocumentType.Value,
                                   referenceType: ReferenceType.Value,
                                   acronym: String,
                                   symbol: String,
                                   references: String) extends ParsedReference
