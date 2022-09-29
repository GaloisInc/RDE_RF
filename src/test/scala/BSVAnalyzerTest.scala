import DocumentEnrichers.BSVDocumentEnricher
import Formatter.InlineFormatter
import TestUtils.TestUtility
import Types.DocumentType
import org.scalatest.flatspec._
import org.scalatest.matchers._

class BSVAnalyzerTest extends AnyFlatSpec with should.Matchers {
  private val formatterType = new InlineFormatter()
  private val documentAnalyser = new BSVDocumentEnricher(formatterType)
  private val expectedDocumentType = DocumentType.BSV
  private val resourceFolder = "../BSV"
  private val testUtility = new TestUtility()

  "BSVDocumentEnricher" should "be able to extract modules from Actuation" in {
    val fileName = "Actuation"
    testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberExprectedSystem = 1)
  }

  "BSVDocumentEnricher" should "be able to extract modules from Actuation_Generated_BVI" in {
    val fileName = "Actuation_Generated_BVI"
    testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberExprectedSystem = 1, numberOfSubSystem = 3)
  }

  "BSVDocumentEnricher" should "be able to extract modules from Instrumentation_Handwritten_BVI" in {
    val fileName = "Instrumentation_Handwritten_BVI"
    testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberExprectedSystem = 1, numberOfSubSystem = 3)
  }

  "BSVDocumentEnricher" should "be able to extract modules from Instrumentation" in {
    val fileName = "Instrumentation"
    testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberExprectedSystem = 1)
  }

  "BSVDocumentEnricher" should "be able to extract modules from Nerv_BVI" in {
    val fileName = "Nerv_BVI"
    testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberExprectedSystem = 1, numberOfSubSystem = 1)
  }

  "BSVDocumentEnricher" should "be able to extract modules from Nerv" in {
    val fileName = "Nerv"
    testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberExprectedSystem = 1, numberOfSubSystem = 1)
  }
}



