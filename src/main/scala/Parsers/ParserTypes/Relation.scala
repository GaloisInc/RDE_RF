package Parsers.ParserTypes

import Parsers.ParserTypes.ParsedReference
import Types.{DocumentType, ReferenceType}

case class Relation(name: String,
                    originalLine: String,
                    documentName: String,
                    documentType: DocumentType.documentType,
                    referenceType: ReferenceType.referenceType,
                    sourceName: String,
                    targetName: String,
                    relationType: String) extends ParsedReference