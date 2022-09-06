import DocumentEnrichers.CryptolDocumentEnricher
import Types.{DocumentType, ReferenceType}
import Utils.{Control, FileUtil}
import Formatter.InlineFormatter
import TestUtils.TestUtility
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import java.io.File
import scala.collection.mutable
import scala.io.Source

class CryptolAnalyzerTest extends AnyFlatSpec with should.Matchers {
  private val fileUtil = FileUtil()
  private val formatterType = InlineFormatter()
  private val documentAnalyser = CryptolDocumentEnricher(formatterType)
  private val expectedDocumentType = DocumentType.Cryptol
  private val resourceFolder = "Cryptol"
  private val testUtility = TestUtility()


  "CryptolDocumentEnricher" should "be able to extract types from AcutationUnit" in {
    val fileName = "ActuationUnit"
    val filePath = getPathToDocument(fileName, resourceFolder)
    testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, filePath, numberOfTypes = 5, numberOfRequirements = 5, numberOfEvents = 9)
  }

  "CryptolDocumentEnricher" should "be able to extract types from Actuator" in {
    val fileName = "Actuator"
    val filePath = getPathToDocument(fileName, resourceFolder)
    testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, filePath, numberOfTypes = 3, numberOfEvents = 3)
  }

  "CryptolDocumentEnricher" should "be able to extract types from InstrumentationUnit" in {
    val fileName = "InstrumentationUnit"
    val filePath = getPathToDocument(fileName, resourceFolder)
    testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, filePath, numberOfTypes = 10, numberOfRequirements = 9, numberOfEvents = 9)
  }

  "CryptolDocumentEnricher" should "be able to extract types from RTS" in {
    val fileName = "RTS"
    val filePath = getPathToDocument(fileName, resourceFolder)
    testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, filePath, numberOfTypes = 10, numberOfRequirements = 6, numberOfEvents = 7)
  }

  "CryptolDocumentEnricher" should "be able to extract types from Utils" in {
    val fileName = "Utils"
    val filePath = getPathToDocument(fileName, resourceFolder)
    testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, filePath, numberOfEvents = 1)
  }

  private def getPathToDocument(fileName: String, resourceFolderName: String): String = {
    val documents = getClass.getResource(resourceFolderName).getPath
    val filesToAnalyze = fileUtil.getListOfFiles(documents).toArray
    val documentOfInterest = filesToAnalyze.filter(path => fileUtil.getFileName(path) == fileName)

    assert(documentOfInterest.length == 1)
    documentOfInterest.head
  }
}



