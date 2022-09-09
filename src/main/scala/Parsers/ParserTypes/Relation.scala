package Parsers.ParserTypes

import Parsers.ParserTypes.ParsedReference
import Types.ReferenceType

case class Relation(name: String, originalLine: String, referenceType: ReferenceType, sourceName: String, targetName: String, relationType: String) extends ParsedReference(name, originalLine, referenceType)
