package Types

final case class DocRelationFromParser(
                                        override val documentName: String,
                                        relationReference: RelationReference,
                                        relationType: RelationTypes.Value,
                                        originalLine: String,
                                      ) extends DocumentReference {
  require(originalLine.nonEmpty, "originalLine must not be empty")
  require(documentName.nonEmpty, "documentName must not be empty")
  require(relationReference != null, "relationReference must not be null")

  def getSourceName: String = relationReference.sourceName

  def getTargetName: String = relationReference.targetName
}
