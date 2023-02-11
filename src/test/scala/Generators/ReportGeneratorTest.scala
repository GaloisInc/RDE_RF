package Generators

import ConfigParser.RefinementModel
import Formatter.InlineFormatter
import Report.PaperLayout
import Types.DocumentType
import Utils.LatexCompilationTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReportGeneratorTest extends AnyFlatSpec with Matchers with LatexCompilationTester {
  private val reportName: String = "Report"
  private val formatter: InlineFormatter = new InlineFormatter()

  "ReportGenerator" should "generate a report" in {
    val fileTypes = Set(DocumentType.SysML, DocumentType.Lando, DocumentType.Cryptol, DocumentType.Fret)
    buildRefinementReport(fileTypes, reportName, PaperLayout.A4, formatter, Set.empty[RefinementModel], sortFiles = false)
  }

  "ReportGenerator" should "generate a report from all files" in {
    val fileTypes = Set(DocumentType.SysML, DocumentType.Lando, DocumentType.Cryptol, DocumentType.Fret, DocumentType.SV, DocumentType.Lobot)
    buildRefinementReport(fileTypes, reportName, PaperLayout.A4, formatter, Set.empty[RefinementModel], sortFiles = false)
  }
}
