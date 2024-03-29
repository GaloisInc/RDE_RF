package Types

final case class ReferenceName(name: String, acronym: Option[String] = None) {
  require(getName.nonEmpty, "Reference name must not be empty")

  override def toString: String = {
    if (acronym.isDefined) {
      s"$name (${acronym.get})"
    } else {
      name
    }
  } ensuring(_.nonEmpty, "Reference name must not be empty")

  def getName: String = if (name.isEmpty && acronym.isDefined) acronym.get else name
}
