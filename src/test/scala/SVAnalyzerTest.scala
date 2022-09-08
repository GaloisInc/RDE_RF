import DocumentEnrichers.SVDocumentEnricher
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

class SVAnalyzerTest extends AnyFlatSpec with should.Matchers {
  private val formatterType = InlineFormatter()
  private val documentAnalyser = SVDocumentEnricher(formatterType)
  private val expectedDocumentType = DocumentType.SV
  private val resourceFolder = "SystemVerilog"
  private val testUtility = new TestUtility()
  private val fileUtil = FileUtil()


  "SVDocumentEnricher" should "be able to extract modules from AcutationUnit" in {
    val fileName = "actuation_unit_impl"
    val filePath = getPathToDocument(fileName, resourceFolder)
    testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, filePath, numberExprectedSystem = 7)
  }

  "SVDocumentEnricher" should "be able to extract modules from Actuator Impl" in {
    val fileName = "actuator_impl"
    val filePath = getPathToDocument(fileName, resourceFolder)
    testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, filePath, numberExprectedSystem = 1)
  }

  "SVDocumentEnricher" should "be able to extract modules from Instrumentation Impl Generated" in {
    val fileName = "instrumentation_impl"
    val filePath = getPathToDocument(fileName, resourceFolder)
    testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, filePath, numberExprectedSystem = 3)
  }

  "SVDocumentEnricher" should "be able to extract modules from Instrumentation Impl Handwritten" in {
    val fileName = "instrumentation_impl_handwritten"
    val filePath = getPathToDocument(fileName, resourceFolder)
    testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, filePath, numberExprectedSystem = 2)
  }

  private def getPathToDocument(fileName: String, resourceFolderName: String): String = {
    val documents = getClass.getResource(resourceFolderName).getPath
    val filesToAnalyze = fileUtil.getListOfFiles(documents).toArray
    val documentOfInterest = filesToAnalyze.filter(path => fileUtil.getFileName(path) == fileName)

    assert(documentOfInterest.length == 1)
    documentOfInterest.head
  }
}



