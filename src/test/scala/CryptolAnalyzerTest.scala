import DocumentEnrichers.CryptolDocumentEnricher
import Types.{DocumentType, ReferenceType}
import Utils.{Control, FileUtil, TestUtility}
import Formatter.InlineFormatter
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

  "CryptolDocumentEnricher" should "be able to extract types from AcutationUnit" in {
    TestUtility.checkExtractReferences("ActuationUnit", documentAnalyser, expectedDocumentType, resourceFolder, numberOfTypes = 5, numberOfRequirements = 5, numberOfEvents = 9)
  }

  "CryptolDocumentEnricher" should "be able to extract types from Actuator" in {
    TestUtility.checkExtractReferences("Actuator", documentAnalyser, expectedDocumentType, resourceFolder, numberOfTypes = 3, numberOfEvents = 3)
  }

  "CryptolDocumentEnricher" should "be able to extract types from InstrumentationUnit" in {
    TestUtility.checkExtractReferences("InstrumentationUnit", documentAnalyser, expectedDocumentType, resourceFolder, numberOfTypes = 10, numberOfRequirements = 9, numberOfEvents = 9)
  }

  "CryptolDocumentEnricher" should "be able to extract types from RTS" in {
    TestUtility.checkExtractReferences("RTS", documentAnalyser, expectedDocumentType, resourceFolder, numberOfTypes = 10, numberOfRequirements = 6, numberOfEvents = 7)
  }

  "CryptolDocumentEnricher" should "be able to extract types from Utils" in {
    TestUtility.checkExtractReferences("Utils", documentAnalyser, expectedDocumentType, resourceFolder, numberOfEvents = 1)
  }
}



