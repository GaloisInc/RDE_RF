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



