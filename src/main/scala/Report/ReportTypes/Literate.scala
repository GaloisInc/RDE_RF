package Report.ReportTypes

final case class Literate(symbol: String, replacement: String) {
  override def toString: String = s"{$symbol} {{$replacement}}1"
}
