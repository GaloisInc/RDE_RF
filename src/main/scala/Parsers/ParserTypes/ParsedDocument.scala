package Parsers.ParserTypes

import Types.DocReference.DocReference
import Types.DocumentInfos.DocumentInfo
import Types.{DocRelation, DocumentType, FileType}


final case class ParsedDocument(
                                 documentName: String,
                                 filePath: String,
                                 references: Set[ParsedReference],
                                 documentType: DocumentType.Value,
                               )
