package ConfigParser

case class RefinementConfig(
                         name: String = "",
                         implicitRefinements: List[String] = Nil,
                         explicitRefinements: List[String] = Nil,
                       )