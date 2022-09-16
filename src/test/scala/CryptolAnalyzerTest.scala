import DocumentEnrichers.CryptolDocumentEnricher
import Types.{DocumentType, ReferenceType}
import Utils.{Control, FileUtil}
import Formatter.InlineFormatter
import TestUtils.TestUtility
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers._

import java.io.File
import scala.collection.mutable
import scala.io.Source

class CryptolAnalyzerTest extends AnyFlatSpec with should.Matchers {
  private val formatterType = new InlineFormatter()
  private val documentAnalyser = new CryptolDocumentEnricher(formatterType)
  private val expectedDocumentType = DocumentType.Cryptol
  private val resourceFolder = "../Cryptol"
  private val testUtility = new TestUtility()

  "CryptolDocumentEnricher" should "be able to extract types from AcutationUnit" in {
    val fileName = "ActuationUnit"
    testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberOfTypes = 5, numberOfRequirements = 5, numberOfEvents = 9)
  }

  "CryptolDocumentEnricher" should "be able to extract types from Actuator" in {
    val fileName = "Actuator"
    testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberOfTypes = 3, numberOfEvents = 3)
  }

  "CryptolDocumentEnricher" should "be able to extract types from InstrumentationUnit" in {
    val fileName = "InstrumentationUnit"
    //testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberOfTypes = 10, numberOfRequirements = 9, numberOfEvents = 9)
  }

  "CryptolDocumentEnricher" should "be able to extract types from RTS" in {
    val fileName = "RTS"
    //testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberOfTypes = 10, numberOfRequirements = 6, numberOfEvents = 7)
  }

  "CryptolDocumentEnricher" should "be able to extract types from Utils" in {
    val fileName = "Utils"
    testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberOfEvents = 1)
  }
}



