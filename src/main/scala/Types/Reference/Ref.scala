package Types.Reference

trait Ref {
  def name: String

  def symbol: String

  require(name.nonEmpty, "Reference name cannot be empty")
  require(symbol.nonEmpty, "Reference symbol cannot be empty")

  def getCleanName: String = name.stripPrefix("'").stripSuffix("'")

}
