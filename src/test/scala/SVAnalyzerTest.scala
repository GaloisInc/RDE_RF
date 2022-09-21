import DocumentEnrichers.SVDocumentEnricher
import Formatter.InlineFormatter
import TestUtils.TestUtility
import Types.DocumentType
import org.scalatest.flatspec._
import org.scalatest.matchers._

class SVAnalyzerTest extends AnyFlatSpec with should.Matchers {
  private val formatterType = new InlineFormatter()
  private val documentAnalyser = new SVDocumentEnricher(formatterType)
  private val expectedDocumentType = DocumentType.SV
  private val resourceFolder = "../SystemVerilog"
  private val testUtility = new TestUtility()


  "SVDocumentEnricher" should "be able to extract modules from AcutationUnit" in {
    val fileName = "actuation_unit_impl"
    testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberExprectedSystem = 7)
  }

  "SVDocumentEnricher" should "be able to extract modules from Actuator Impl" in {
    val fileName = "actuator_impl"
    testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberExprectedSystem = 1)
  }

  "SVDocumentEnricher" should "be able to extract modules from Instrumentation Impl Generated" in {
    val fileName = "instrumentation_impl"
    testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberExprectedSystem = 3)
  }

  "SVDocumentEnricher" should "be able to extract modules from Instrumentation Impl Handwritten" in {
    val fileName = "instrumentation_impl_handwritten"
    testUtility.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberExprectedSystem = 2)
  }
}



