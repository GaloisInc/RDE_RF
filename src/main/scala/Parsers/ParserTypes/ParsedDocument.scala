package Parsers.ParserTypes

import Types.DocumentType


final case class ParsedDocument(
                                 documentName: String,
                                 filePath: String,
                                 references: Set[ParsedReference],
                                 documentType: DocumentType.Value,
                               )
