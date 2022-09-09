package Types.Reference

case class TypeRef(override val name: String, override val symbol: String) extends Ref {
  require(name.nonEmpty, "Type name cannot be empty")
  require(symbol.nonEmpty, "Type symbol cannot be empty")
  require(symbol == ":", "Type symbol must be ':'")
}
