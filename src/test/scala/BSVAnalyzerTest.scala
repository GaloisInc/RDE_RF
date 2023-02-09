import DocumentEnrichers.BSVDocumentEnricher
import Formatter.InlineFormatter
import Types.DocumentType
import Utils.TestUtilityBSV
import org.scalatest.flatspec._
import org.scalatest.matchers._

class BSVAnalyzerTest extends AnyFlatSpec with should.Matchers {
  private val formatterType = new InlineFormatter()
  private val documentAnalyser: BSVDocumentEnricher = new BSVDocumentEnricher(formatterType)
  private val expectedDocumentType = DocumentType.BSV
  private val resourceFolder = "../BSV"

  "BSVDocumentEnricher" should "be able to extract modules from Actuation" in {
    val fileName = "Actuation"
    TestUtilityBSV.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberExpectedSystem = 1)
  }

  "BSVDocumentEnricher" should "be able to extract modules from Actuation_Generated_BVI" in {
    val fileName = "Actuation_Generated_BVI"
    TestUtilityBSV.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberExpectedSystem = 1, numberOfSubSystem = 3)
  }

  "BSVDocumentEnricher" should "be able to extract modules from Instrumentation_Handwritten_BVI" in {
    val fileName = "Instrumentation_Handwritten_BVI"
    TestUtilityBSV.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberExpectedSystem = 1, numberOfSubSystem = 3)
  }

  "BSVDocumentEnricher" should "be able to extract modules from Instrumentation" in {
    val fileName = "Instrumentation"
    TestUtilityBSV.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberExpectedSystem = 1)
  }

  "BSVDocumentEnricher" should "be able to extract modules from Nerv_BVI" in {
    val fileName = "Nerv_BVI"
    TestUtilityBSV.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberExpectedSystem = 1, numberOfSubSystem = 1)
  }

  "BSVDocumentEnricher" should "be able to extract modules from Nerv" in {
    val fileName = "Nerv"
    TestUtilityBSV.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberExpectedSystem = 1, numberOfSubSystem = 1)
  }
}



