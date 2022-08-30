package Types

final case class DocRelation(
                        documentName: String,
                        explicit: Boolean,
                        relationReference: RelationReference,
                        relationType: RelationType,
                        originalLine: String,
                        enrichedLine: Option[String]
                      ) extends EnrichableString
