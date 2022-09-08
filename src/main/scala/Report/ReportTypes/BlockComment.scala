package Report.ReportTypes

final case class BlockComment(startSymbol: String, endSymbol: String){
  require(startSymbol.length == endSymbol.length, "Start and end symbols must be the same length")
  require(startSymbol.nonEmpty, "Start and end symbols must be non-empty")
  require(!startSymbol.equalsIgnoreCase(endSymbol), "Start and end symbols must be different")
}
