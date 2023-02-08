package Report

import Analyzers.{DocumentAnalyzer, LatexDocumentData}
import ConfigParser.RefinementModel
import Formatter.InlineFormatter
import Utils.FileUtil
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

class ReportGeneratorTest extends AnyFlatSpec with Matchers {
  "ReportGenerator" should "generate a report" in {
    val sysmlDocuments = getClass.getResource("../SysML").getPath
    val landoDocuments = getClass.getResource("../Lando").getPath
    val cryptolDocuments = getClass.getResource("../Cryptol").getPath

    val filesToAnalyze = FileUtil.getFilesInDirectory(sysmlDocuments) ++
      FileUtil.getFilesInDirectory(landoDocuments) ++
      FileUtil.getFilesInDirectory(cryptolDocuments)

    val reportName = "Report"
    val reportPath = getClass.getResource(".").getPath
    val latexDocumentData = LatexDocumentData(reportName, reportPath, PaperLayout.A4, new InlineFormatter())

    val report = DocumentAnalyzer.generateReport(filesToAnalyze, latexDocumentData, Set.empty[RefinementModel],sortFiles = false)

    val reportFilePath = RefinementReport.buildReport(report)

    //Ensure that the report is generated
    val reportFile = new File(reportFilePath)
    reportFile.exists() should be(true)
    reportFile.delete()
  }
}
