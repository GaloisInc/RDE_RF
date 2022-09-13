package Parsers.ParserTypes

import Types.{ReferenceType, DocumentType}

case class ParsedRefWithReferences(name: String,
                                   originalLine: String,
                                   documentName: String,
                                   documentType: DocumentType,
                                   referenceType: ReferenceType,
                                   acronym: String,
                                   symbol: String,
                                   references: String) extends ParsedReference(name, originalLine, referenceType, documentName, documentType)
