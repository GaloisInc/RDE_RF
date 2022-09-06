import DocumentEnrichers.SysMLDocumentEnricher
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

class SysMLAnalyzerTest extends AnyFlatSpec with should.Matchers {
  private val formatterType = InlineFormatter()
  private val documentEnricher = SysMLDocumentEnricher(formatterType)
  private val expectedDocumentType = DocumentType.SysML
  private val fileUtil = FileUtil()
  private val resourceFolder = "SysML"
  private val testUtility = TestUtility()

  "SysMLReader" should "to extract parts and items" in {
    val fileName = "RTS_Glossary"
    val filePath = getPathToDocument(fileName, resourceFolder)
    testUtility.checkExtractReferences("RTS_Glossary", documentEnricher, expectedDocumentType, filePath, 1, 80, 66, 0, 0, 0, 0, 0)
  }

  "SysMLReader" should "to extract requirements" in {
    val fileName = "RTS_Requirements"
    val filePath = getPathToDocument(fileName, resourceFolder)
    testUtility.checkExtractReferences(fileName, documentEnricher, expectedDocumentType, filePath, 4, 0, 0, 17, 0, 0, 0, 0)
  }

  "SysMLReader" should "to extract packages" in {
    val fileName = "HARDENS"
    val filePath = getPathToDocument(fileName, resourceFolder)
    testUtility.checkExtractReferences(fileName, documentEnricher, expectedDocumentType, filePath, 7, 0, 0, 0, 0, 0, 0, 0)
  }

  "SysMLReader" should "to extract Scenarios" in {
    val fileName = "RTS_Scenarios"
    val filePath = getPathToDocument(fileName, resourceFolder)
    testUtility.checkExtractReferences(fileName, documentEnricher, expectedDocumentType, filePath, 4, 0, 1, 0, 39, 0, 0, 0)
  }

  "SysMLReader" should "to extract Views" in {
    val fileName = "RTS_Viewpoints"
    val filePath = getPathToDocument(fileName, resourceFolder)
    testUtility.checkExtractReferences(fileName, documentEnricher, expectedDocumentType, filePath, numberOfSystem = 1, numberOfViews = 8)
  }

  "SysMLReader" should "to extract Actions" in {
    val fileName = "RTS_Actions"
    val filePath = getPathToDocument(fileName, resourceFolder)
    testUtility.checkExtractReferences(fileName, documentEnricher, expectedDocumentType, filePath, numberOfSystem = 4, numberOfEvents = 20)
  }

  private def getPathToDocument(fileName: String, resourceFolderName: String): String = {
    val documents = getClass.getResource(resourceFolderName).getPath
    val filesToAnalyze = fileUtil.getListOfFiles(documents).toArray
    val documentOfInterest = filesToAnalyze.filter(path => fileUtil.getFileName(path) == fileName)

    assert(documentOfInterest.length == 1)
    documentOfInterest.head
  }
}



