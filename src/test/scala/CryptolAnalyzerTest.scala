import DocumentEnrichers.CryptolDocumentEnricher
import Formatter.InlineFormatter
import Types.DocumentType
import Utils.TestUtilityCryptol
import org.scalatest.flatspec._
import org.scalatest.matchers._

class CryptolAnalyzerTest extends AnyFlatSpec with should.Matchers {
  private val formatterType = new InlineFormatter()
  private val documentAnalyser = new CryptolDocumentEnricher(formatterType)
  private val expectedDocumentType = DocumentType.Cryptol
  private val resourceFolder = "../Cryptol"

  "CryptolDocumentEnricher" should "be able to extract types from AcutationUnit" in {
    val fileName = "ActuationUnit"
    TestUtilityCryptol.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberOfTypes = 5, numberOfRequirements = 5, numberOfEvents = 9)
  }

  "CryptolDocumentEnricher" should "be able to extract types from Actuator" in {
    val fileName = "Actuator"
    TestUtilityCryptol.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberOfTypes = 3, numberOfEvents = 3)
  }

  "CryptolDocumentEnricher" should "be able to extract types from InstrumentationUnit" in {
    val fileName = "InstrumentationUnit"
    TestUtilityCryptol.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberOfTypes = 10, numberOfRequirements = 9, numberOfEvents = 30)
  }

  "CryptolDocumentEnricher" should "be able to extract types from RTS" in {
    val fileName = "RTS"
    TestUtilityCryptol.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberOfTypes = 10, numberOfRequirements = 6, numberOfEvents = 18)
  }

  "CryptolDocumentEnricher" should "be able to extract types from Utils" in {
    val fileName = "Utils"
    TestUtilityCryptol.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberOfEvents = 1)
  }
}



