package Report

import Analyzers.DocumentAnalyzer
import Utils.FileUtil
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

class ReportGeneratorTest extends AnyFlatSpec with Matchers {
  "ReportGenerator" should "generate a report" in {
    val sysmlDocuments = getClass.getResource("../SysML").getPath
    val landoDocuments = getClass.getResource("../Lando").getPath
    val cryptolDocuments = getClass.getResource("../Cryptol").getPath

    val filesToAnalyze = FileUtil.getListOfFiles(sysmlDocuments).toArray
      ++ FileUtil.getListOfFiles(landoDocuments).toArray
      ++ FileUtil.getListOfFiles(cryptolDocuments).toArray

    val reportName = "Report"
    val reportPath = getClass.getResource(".").getPath
    val report = DocumentAnalyzer.generateReport(filesToAnalyze, reportName, reportPath, false)

    val reportFilePath = ReportGenerator.generateRefinementReport(report)

    //Ensure that the report is generated
    val reportFile = new File(reportFilePath)
    reportFile.exists() should be(true)
    reportFile.delete()
  }
}
