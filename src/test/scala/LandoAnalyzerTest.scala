import DocumentEnrichers.LandoDocumentEnricher
import Formatter.InlineFormatter
import TestUtils.TestUtility
import Types.{DocumentType, ReferenceType}
import Utils.{Control, FileUtil}
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import java.io.File
import scala.collection.mutable
import scala.io.Source

class LandoAnalyzerTest extends AnyFlatSpec with should.Matchers {
  private val formatterType = InlineFormatter()
  private val landoDocumentEnricher = LandoDocumentEnricher(formatterType)
  private val expectedDocumentType = DocumentType.Lando
  private val resourceFolder = "lando"
  private val testUtility = TestUtility()
  private val fileUtil = FileUtil()

  "LandoDocumentEnricher" should "be able to extract glossary" in {
    val fileName = "glossary"
    val filePath = getPathToDocument(fileName, resourceFolder)
    testUtility.checkExtractReferences(fileName, landoDocumentEnricher, expectedDocumentType, filePath, 0, 1, 104)
  }

  "LandoDocumentEnricher" should "be able to extract system" in {
    val fileName = "RTS"
    val filePath = getPathToDocument(fileName, resourceFolder)
    testUtility.checkExtractReferences(fileName, landoDocumentEnricher, expectedDocumentType, filePath, 1, 6)
  }

  "LandoDocumentEnricher" should "be able to extract events" in {
    val fileName = "events"
    val filePath = getPathToDocument(fileName, resourceFolder)
    testUtility.checkExtractReferences(fileName, landoDocumentEnricher, expectedDocumentType, filePath, numberOfEvents = 16)
  }

  "LandoDocumentEnricher" should "be able to extract requirements" in {
    val fileName = "project_requirements"
    val filePath = getPathToDocument(fileName, resourceFolder)
    testUtility.checkExtractReferences(fileName, landoDocumentEnricher, expectedDocumentType, filePath, 0, 0, 0, 16, 0, 0, 0, 0)
  }

  "LandoDocumentEnricher" should "be able to extract scenarios" in {
    val fileName = "test_scenarios"
    val filePath = getPathToDocument(fileName, resourceFolder)
    testUtility.checkExtractReferences(fileName, landoDocumentEnricher, expectedDocumentType, filePath, numberOfScenarios = 40)
  }

  private def getPathToDocument(fileName: String, resourceFolderName: String): String = {
    val documents = getClass.getResource(resourceFolderName).getPath
    val filesToAnalyze = fileUtil.getListOfFiles(documents).toArray
    val documentOfInterest = filesToAnalyze.filter(path => fileUtil.getFileName(path) == fileName)

    assert(documentOfInterest.length == 1)
    documentOfInterest.head
  }
}



