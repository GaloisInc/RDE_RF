package ConfigParser

case class RefinementConfig(
                         name: String = "",
                         refinements: List[String] = Nil,
                       )