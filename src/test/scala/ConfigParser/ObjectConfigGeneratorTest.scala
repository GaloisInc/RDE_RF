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
    val filesToAnalyze = FileUtil.getFilesInDirectory(landoDocuments).toArray

    assert(filesToAnalyze.length == 1)
    val filePath = filesToAnalyze.head
    val title = "Lando"
    val folder = getClass.getResource("./").getPath
    val documentInfo = documentAnalyzer.parseDocument(filePath)
    val report = ReportReference(title, folder, Array(documentInfo), Array.empty, Array.empty, Array.empty, Array.empty, PaperLayout.A4)

    val reportFilePath = ObjectConfigGenerator.generateRefinementConfigFile(report, "test")

    reportFilePath should not be null
    val file = new File(reportFilePath)
    file.exists() should be(true)
    val loadedFile = RefinementLoader.load(reportFilePath)
    loadedFile.name should be("test")
    loadedFile.explicit_refinements.size should be(1)
    file.delete()
  }

  "ObjectConfigGenerator" should "generate a config file from documents" in {
    val sysmlDocuments = getClass.getResource("../SysML").getPath
    val landoDocuments = getClass.getResource("../Lando").getPath
    val cryptolDocuments = getClass.getResource("../Cryptol").getPath
    val bsvDocuments = getClass.getResource("../BSV").getPath
    val svDocuments = getClass.getResource("../SystemVerilog").getPath


    val filesToAnalyze = FileUtil.getFilesInDirectory(sysmlDocuments).toArray ++
      FileUtil.getFilesInDirectory(landoDocuments).toArray ++
      FileUtil.getFilesInDirectory(cryptolDocuments).toArray ++
      FileUtil.getFilesInDirectory(bsvDocuments).toArray ++
      FileUtil.getFilesInDirectory(svDocuments).toArray

    val reportName = "Report_Refinements"
    val reportPath = getClass.getResource(".").getPath
    val latexDocumentData = LatexDocumentData(reportName, reportPath, PaperLayout.A4, new InlineFormatter())

    val report = DocumentAnalyzer.generateReport(filesToAnalyze.toSet, latexDocumentData, Set.empty[RefinementModel], false)
    val reportFilePath = ObjectConfigGenerator.generateRefinementConfigFile(report, "test_explicit")

    reportFilePath should not be null
    val file = new File(reportFilePath)
    file.exists() should be(true)


    val loadedFile = RefinementLoader.load(reportFilePath)
    loadedFile.name should be(reportName)
    loadedFile.explicit_refinements.values.size should be(1)

    //file.delete()
  }

  "ObjectConfigGenerator" should "generate a config file woth none refined references from documents" in {
    val sysmlDocuments = getClass.getResource("../SysML").getPath
    val landoDocuments = getClass.getResource("../Lando").getPath
    val cryptolDocuments = getClass.getResource("../Cryptol").getPath
    val bsvDocuments = getClass.getResource("../BSV").getPath
    val svDocuments = getClass.getResource("../SystemVerilog").getPath


    val filesToAnalyze = FileUtil.getFilesInDirectory(sysmlDocuments).toArray ++
      FileUtil.getFilesInDirectory(landoDocuments).toArray ++
      FileUtil.getFilesInDirectory(cryptolDocuments).toArray ++
      FileUtil.getFilesInDirectory(bsvDocuments).toArray ++
      FileUtil.getFilesInDirectory(svDocuments).toArray

    val reportName = "Report_Refinements_None"
    val reportPath = getClass.getResource(".").getPath
    val latexDocumentData = LatexDocumentData(reportName, reportPath, PaperLayout.A4, new InlineFormatter())

    val report = DocumentAnalyzer.generateReport(filesToAnalyze.toSet, latexDocumentData, Set.empty[RefinementModel],false)
    val reportFilePath = ObjectConfigGenerator.generateRefinementConfigFile(report, "test_explicit_none")

    reportFilePath should not be null
    val file = new File(reportFilePath)
    file.exists() should be(true)
  }

}
