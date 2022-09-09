package Parsers.ParserTypes

import Types.ReferenceType

case class ParsedRefWithReferences(name: String, originalLine: String, referenceType: ReferenceType, acronym: String, symbol: String, references: String) extends ParsedReference(name, originalLine, referenceType)
