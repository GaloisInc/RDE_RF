package Types

case class DocReference(
                         documentName: String,
                         referenceName: ReferenceName,
                         referenceType: ReferenceType,
                         documentType: DocumentType,
                         originalLine: String,
                         enrichedLine: Option[String] = None,
                         abstracts: Option[Set[DocReference]] = None,
                         specializes: Option[Set[DocReference]] = None,
                       ) extends EnrichableString
