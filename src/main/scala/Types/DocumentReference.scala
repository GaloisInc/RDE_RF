package Types

trait DocumentReference {
  def documentName: String

  require(documentName.nonEmpty, "documentName must not be empty")
}
