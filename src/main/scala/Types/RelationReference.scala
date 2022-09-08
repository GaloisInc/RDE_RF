package Types

final case class RelationReference(
                              sourceName: String,
                              targetName: String,
                            ){

  require(sourceName != targetName, "sourceName and targetName must be different")
  require(sourceName.nonEmpty, "sourceName must not be empty")
  require(targetName.nonEmpty, "targetName must not be empty")
  override def toString: String = s"$sourceName -> $targetName"
}
