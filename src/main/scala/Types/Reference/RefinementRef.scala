package Types.Reference

import Types.Reference.Ref

case class RefinementRef(override val name: String, override val symbol: String) extends Ref {
  require(name.nonEmpty, "Type name cannot be empty")
  require(symbol.nonEmpty, "Type symbol cannot be empty")
  require(symbol == ":>", "Type symbol must be ':>'")
}
