package ConfigParser

import Analyzers.{DocumentAnalyzer, LatexDocumentData}
import DocumentEnrichers.LandoDocumentEnricher
import Formatter.InlineFormatter
import Report.PaperLayout
import Report.ReportTypes.ReportReference
import Utils.FileUtil
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.io.File

class ObjectConfigGeneratorTest extends AnyFlatSpec with should.Matchers {

  "ObjectConfigGenerator" should "generate a config object" in {
    val documentAnalyzer = new LandoDocumentEnricher(new InlineFormatter())
    val landoDocuments = getClass.getResource("../lando_changed").getPath
    val filesToAnalyze = FileUtil.getListOfFiles(landoDocuments).toArray

    assert(filesToAnalyze.length == 1)
    val filePath = filesToAnalyze.head
    val title = "Lando"
    val folder = getClass.getResource("./").getPath
    val documentInfo = documentAnalyzer.parseDocument(filePath)
    val report = ReportReference(title, folder, Array(documentInfo), Array.empty, Array.empty, Array.empty, Array.empty, PaperLayout.A4)

    val reportFilePath = ObjectConfigGenerator.generateNoneRefinedFile(report)

    reportFilePath should not be null
    val file = new File(reportFilePath)
    file.exists() should be(true)
    val loadedFile = RefinementLoader.load(reportFilePath)
    loadedFile.name should be(title)
    loadedFile.refinements.length should be(1)
    file.delete()
  }

  "ObjectConfigGenerator" should "generate a config file from documents" in {
    val sysmlDocuments = getClass.getResource("../SysML").getPath
    val landoDocuments = getClass.getResource("../Lando").getPath
    val cryptolDocuments = getClass.getResource("../Cryptol").getPath
    val bsvDocuments = getClass.getResource("../BSV").getPath
    val svDocuments = getClass.getResource("../SystemVerilog").getPath


    val filesToAnalyze = FileUtil.getListOfFiles(sysmlDocuments).toArray ++
      FileUtil.getListOfFiles(landoDocuments).toArray ++
      FileUtil.getListOfFiles(cryptolDocuments).toArray ++
      FileUtil.getListOfFiles(bsvDocuments).toArray ++
      FileUtil.getListOfFiles(svDocuments).toArray

    val reportName = "Report_Refinements"
    val reportPath = getClass.getResource(".").getPath
    val latexDocumentData = LatexDocumentData(reportName, reportPath, PaperLayout.A4, new InlineFormatter())

    val report = DocumentAnalyzer.generateReport(filesToAnalyze, latexDocumentData, false)
    val reportFilePath = ObjectConfigGenerator.generateRefinedFile(report)

    reportFilePath should not be null
    val file = new File(reportFilePath)
    file.exists() should be(true)


    val loadedFile = RefinementLoader.load(reportFilePath)
    loadedFile.name should be(reportName)
    loadedFile.refinements.length should be(1)

    file.delete()
  }

  "ObjectConfigGenerator" should "generate a config file woth none refined references from documents" in {
    val sysmlDocuments = getClass.getResource("../SysML").getPath
    val landoDocuments = getClass.getResource("../Lando").getPath
    val cryptolDocuments = getClass.getResource("../Cryptol").getPath
    val bsvDocuments = getClass.getResource("../BSV").getPath
    val svDocuments = getClass.getResource("../SystemVerilog").getPath


    val filesToAnalyze = FileUtil.getListOfFiles(sysmlDocuments).toArray ++
      FileUtil.getListOfFiles(landoDocuments).toArray ++
      FileUtil.getListOfFiles(cryptolDocuments).toArray ++
      FileUtil.getListOfFiles(bsvDocuments).toArray ++
      FileUtil.getListOfFiles(svDocuments).toArray

    val reportName = "Report_Refinements_None"
    val reportPath = getClass.getResource(".").getPath
    val latexDocumentData = LatexDocumentData(reportName, reportPath, PaperLayout.A4, new InlineFormatter())

    val report = DocumentAnalyzer.generateReport(filesToAnalyze, latexDocumentData, false)
    val reportFilePath = ObjectConfigGenerator.generateNoneRefinedFile(report)

    reportFilePath should not be null
    val file = new File(reportFilePath)
    file.exists() should be(true)
  }

}
