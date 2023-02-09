import DocumentEnrichers.SVDocumentEnricher
import Formatter.InlineFormatter
import Types.DocumentType
import Utils.TestUtilitySV
import org.scalatest.flatspec._
import org.scalatest.matchers._

class SVAnalyzerTest extends AnyFlatSpec with should.Matchers {
  private val formatterType = new InlineFormatter()
  private val documentAnalyser = new SVDocumentEnricher(formatterType)
  private val expectedDocumentType = DocumentType.SV
  private val resourceFolder = "../SystemVerilog"


  "SVDocumentEnricher" should "be able to extract modules from ActuationUnit" in {
    val fileName = "actuation_unit_impl"
    TestUtilitySV.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberExpectedSystem = 7)
  }

  "SVDocumentEnricher" should "be able to extract modules from Actuator Impl" in {
    val fileName = "actuator_impl"
    TestUtilitySV.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberExpectedSystem = 1)
  }

  "SVDocumentEnricher" should "be able to extract modules from Instrumentation Impl Generated" in {
    val fileName = "instrumentation_impl"
    TestUtilitySV.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberExpectedSystem = 3)
  }

  "SVDocumentEnricher" should "be able to extract modules from Instrumentation Impl Handwritten" in {
    val fileName = "instrumentation_impl_handwritten"
    TestUtilitySV.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, resourceFolder, numberExpectedSystem = 2)
  }
}



