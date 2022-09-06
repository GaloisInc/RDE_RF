import DocumentEnrichers.BSVDocumentEnricher
import Types.{DocumentType, ReferenceType}
import Utils.{Control, FileUtil}
import TestUtils.TestUtility
import Formatter.InlineFormatter
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import java.io.File
import scala.collection.mutable
import scala.io.Source

class BSVAnalyzerTest extends AnyFlatSpec with should.Matchers {
  private val fileUtil = FileUtil()
  private val formatterType = InlineFormatter()
  private val documentAnalyser = BSVDocumentEnricher(formatterType)
  private val expectedDocumentType = DocumentType.BSV
  private val resourceFolder = "BSV"
  private val testUtility = TestUtility()

  "BSVDocumentEnricher" should "be able to extract modules from Actuation" in {
    val fileName = "Actuation"
    val filePath = getPathToDocument(fileName, resourceFolder)
    testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, filePath, numberOfSystem = 1)
  }

  "BSVDocumentEnricher" should "be able to extract modules from Actuation_Generated_BVI" in {
    val fileName = "Actuation_Generated_BVI"
    val filePath = getPathToDocument(fileName, resourceFolder)
    testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, filePath, numberOfSystem = 1, numberOfSubSystem = 3)
  }

  "BSVDocumentEnricher" should "be able to extract modules from Instrumentation_Handwritten_BVI" in {
    val fileName = "Instrumentation_Handwritten_BVI"
    val filePath = getPathToDocument(fileName, resourceFolder)
    testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, filePath, numberOfSystem = 1, numberOfSubSystem = 3)
  }

  "BSVDocumentEnricher" should "be able to extract modules from Instrumentation" in {
    val fileName = "Instrumentation"
    val filePath = getPathToDocument(fileName, resourceFolder)
    testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, filePath, numberOfSystem = 1)
  }

  "BSVDocumentEnricher" should "be able to extract modules from Nerv_BVI" in {
    val fileName = "Nerv_BVI"
    val filePath = getPathToDocument(fileName, resourceFolder)
    testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, filePath, numberOfSystem = 1, numberOfSubSystem = 1)
  }

  "BSVDocumentEnricher" should "be able to extract modules from Nerv" in {
    val fileName = "Nerv"
    val filePath = getPathToDocument(fileName, resourceFolder)
    testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, filePath, numberOfSystem = 1, numberOfSubSystem = 1)
  }

  private def getPathToDocument(fileName: String, resourceFolderName: String): String = {
    val documents = getClass.getResource(resourceFolderName).getPath
    val filesToAnalyze = fileUtil.getListOfFiles(documents).toArray
    val documentOfInterest = filesToAnalyze.filter(path => fileUtil.getFileName(path) == fileName)

    assert(documentOfInterest.length == 1)
    documentOfInterest.head
  }
}



