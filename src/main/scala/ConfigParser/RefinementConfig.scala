package ConfigParser

final case class RefinementFileConfig(
                         name: String = "",
                         implicitRefinements: Map[String, List[String]] = Map.empty,
                         explicitRefinements: Map[String, List[String]] = Map.empty,
                       )