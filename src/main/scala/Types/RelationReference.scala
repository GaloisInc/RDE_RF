package Types

final case class RelationReference(
                              sourceName: String,
                              targetName: String,
                            ){

  require(sourceName.nonEmpty, "sourceName must not be empty")
  require(targetName.nonEmpty, "targetName must not be empty")
  require(sourceName != targetName, "sourceName and targetName must be different")

  override def toString: String = s"$sourceName -> $targetName"
}
