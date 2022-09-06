import DocumentEnrichers.SVDocumentEnricher
import Types.{DocumentType, ReferenceType}
import Utils.{Control, FileUtil}
import Formatter.InlineFormatter
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

  "SVDocumentEnricher" should "be able to extract modules from AcutationUnit" in {
    TestUtility.checkExtractReferences("actuation_unit_impl", documentAnalyser, expectedDocumentType, resourceFolder, numberOfSystem = 7)
  }

  "SVDocumentEnricher" should "be able to extract modules from Actuator Impl" in {
    TestUtility.checkExtractReferences("actuator_impl", documentAnalyser, expectedDocumentType, resourceFolder, numberOfSystem = 1)
  }

  "SVDocumentEnricher" should "be able to extract modules from Instrumentation Impl Generated" in {
    TestUtility.checkExtractReferences("instrumentation_impl", documentAnalyser, expectedDocumentType, resourceFolder, numberOfSystem = 3)
  }

  "SVDocumentEnricher" should "be able to extract modules from Instrumentation Impl Handwritten" in {
    TestUtility.checkExtractReferences("instrumentation_impl_handwritten", documentAnalyser, expectedDocumentType, resourceFolder, numberOfSystem = 2)
  }
}



