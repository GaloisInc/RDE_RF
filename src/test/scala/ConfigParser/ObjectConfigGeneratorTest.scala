package ConfigParser

import Analyzers.{DocumentAnalyzer, LatexDocumentData}
import Formatter.InlineFormatter
import Report.PaperLayout
import Report.ReportTypes.{Documents, ReportReference}
import Types.DocumentInfos.{CryptolDocumentInfo, LandoDocumentInfo, LobotDocumentInfo, SysMLDocumentInfo}
import Types.DocumentType
import Utils.{FileUtil, ResourceFiles}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.io.File

class ObjectConfigGeneratorTest extends AnyFlatSpec with should.Matchers  {
  private val authorName: String = "TestAuthor"

  "ObjectConfigGenerator" should "generate a config object" in {
    val landoDocuments = getClass.getResource("../lando_changed").getPath
    val filesToAnalyze = FileUtil.getFilesInDirectory(landoDocuments).toArray

    assert(filesToAnalyze.length == 1)
    val title = "Lando"
    val folder = getClass.getResource("./").getPath
    val emptyDocument = Documents(Array.empty[LandoDocumentInfo], Array.empty[LobotDocumentInfo], Array.empty[SysMLDocumentInfo], Array.empty[CryptolDocumentInfo], Array.empty, Array.empty, Array.empty, Array.empty, Array.empty)
    val report = ReportReference(title, authorName, folder, emptyDocument, PaperLayout.A4)

    val reportFilePath = ConfigGenerator.generateRefinementConfigFile(report, "test")

    reportFilePath should not be null
    val file = new File(reportFilePath)
    file.exists() should be(true)
    val loadedFile = RefinementLoader.load(reportFilePath)
    loadedFile.name should be("test")
    loadedFile.explicit_refinements.size should be(0)
    file.delete()
  }

  //Test is ignored because there is a problem with the test file
  ignore should "generate a config file from documents" in {
    val filesToAnalyze = ResourceFiles.getFilesOfTypes(Set(DocumentType.SysML, DocumentType.Lando, DocumentType.Cryptol, DocumentType.BSV, DocumentType.SV))

    val filesOfSupportTypes = filesToAnalyze.filter(file => Analyzers.AnalyzerSettings.supportedDocumentTypesString.contains(file.split('.').last))

    val reportName = "Report_Refinements"
    val reportPath = getClass.getResource(".").getPath
    val latexDocumentData = LatexDocumentData(reportName, authorName, reportPath, PaperLayout.A4, new InlineFormatter())

    val report = DocumentAnalyzer.generateReport(filesOfSupportTypes, latexDocumentData, Set.empty[RefinementModel], sortFiles = false)
    val reportFilePath = ConfigGenerator.generateRefinementConfigFile(report, "test_explicit")

    reportFilePath should not be null
    val file = new File(reportFilePath)
    file.exists() should be(true)

    val loadedFile = RefinementLoader.load(reportFilePath)
    loadedFile.name should be(reportName)
    loadedFile.explicit_refinements.values.size should be(1)
  }

  "ObjectConfigGenerator" should "generate a config file with none refined references from documents" in {
    val filesToAnalyze = ResourceFiles.getFilesOfTypes(Set(DocumentType.SysML, DocumentType.Lando, DocumentType.Cryptol, DocumentType.BSV, DocumentType.SV))
    val supportedTypes = filesToAnalyze.filter(f => Analyzers.AnalyzerSettings.supportedDocumentTypesString.contains(FileUtil.getFileType(f)))

    val reportName = "Report_Refinements_None"
    val reportPath = getClass.getResource(".").getPath
    val latexDocumentData = LatexDocumentData(reportName, authorName, reportPath, PaperLayout.A4, new InlineFormatter())

    val report = DocumentAnalyzer.generateReport(supportedTypes, latexDocumentData, Set.empty[RefinementModel], sortFiles = false)
    val reportFilePath = ConfigGenerator.generateRefinementConfigFile(report, "test_explicit_none")

    reportFilePath should not be null
    val file = new File(reportFilePath)
    file.exists() should be(true)
  }

}
