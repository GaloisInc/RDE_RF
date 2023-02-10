package ConfigParser

trait SerializableConfig {
  def serialize(indentationLevel: Int): String

  protected def computeIndentation(indentationLevel: Int): String = {
    "  " * indentationLevel
  }
}

final case class RefinementFileConfig(
                                       name: String = "",
                                       implicitRefinements: Map[String, List[String]] = Map.empty,
                                       explicitRefinements: Map[String, List[String]] = Map.empty,
                                     ) extends SerializableConfig {

  private def serializeMap(indentationLevel: Int, documentRefs: Map[String, List[String]]): String = {
    val indentation = super.computeIndentation(indentationLevel)
    documentRefs.map { case (key, value) =>
      s"""$indentation$key =
         |$indentation[
         |${value.mkString(indentation * 2, s",\n${indentation * 2}", "")}
         |$indentation]""".stripMargin
    }.mkString(",\n")
  }

  private def serializeName(indentationLevel: Int): String = {
    val indentation = super.computeIndentation(indentationLevel)
    s"""${indentation}name = "$name" """
  }

  def serialize(indentationLevel: Int): String = {
    val indentation = super.computeIndentation(indentationLevel)
    val implicitRefinementsString = serializeMap(indentationLevel + 1, implicitRefinements)
    val explicitRefinementsString = serializeMap(indentationLevel + 1, explicitRefinements)
    s"""${serializeName(indentationLevel)},
       |${indentation}implicit-refinements = {
       |$indentation$implicitRefinementsString
       |$indentation},
       |${indentation}explicit-refinements = {
       |$indentation$explicitRefinementsString
       |$indentation}
       |""".stripMargin

  }
}