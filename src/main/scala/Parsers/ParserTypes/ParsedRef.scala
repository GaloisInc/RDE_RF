package Parsers.ParserTypes

import Parsers.ParserTypes.ParsedReference
import Types.ReferenceType

case class ParsedRef(name: String, originalLine: String, referenceType: ReferenceType, acronym: String) extends ParsedReference(name, originalLine, referenceType)


