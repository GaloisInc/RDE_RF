import DocumentEnrichers.BSVDocumentEnricher
import Types.{DocumentType, ReferenceType}
import Utils.{Control, FileUtil, TestUtility}
import Formatter.InlineFormatter
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import java.io.File
import scala.collection.mutable
import scala.io.Source

class BSVAnalyzerTest extends AnyFlatSpec with should.Matchers {
  private val formatterType = InlineFormatter()
  private val documentAnalyser = BSVDocumentEnricher(formatterType)
  private val expectedDocumentType = DocumentType.BSV
  private val resourceFolder = "BSV"


  "BSVDocumentEnricher" should "be able to extract modules from Actuation" in {
    TestUtility.checkExtractReferences("Actuation", documentAnalyser, expectedDocumentType, resourceFolder, numberOfSystem = 1)
  }

  "BSVDocumentEnricher" should "be able to extract modules from Actuation_Generated_BVI" in {
    TestUtility.checkExtractReferences("Actuation_Generated_BVI", documentAnalyser, expectedDocumentType, resourceFolder, numberOfSystem = 1, numberOfSubSystem = 3)
  }

  "BSVDocumentEnricher" should "be able to extract modules from Instrumentation_Handwritten_BVI" in {
    TestUtility.checkExtractReferences("Instrumentation_Handwritten_BVI", documentAnalyser, expectedDocumentType, resourceFolder, numberOfSystem = 1, numberOfSubSystem = 3)
  }

  "BSVDocumentEnricher" should "be able to extract modules from Instrumentation" in {
    TestUtility.checkExtractReferences("Instrumentation", documentAnalyser, expectedDocumentType, resourceFolder, numberOfSystem = 1)
  }

  "BSVDocumentEnricher" should "be able to extract modules from Nerv_BVI" in {
    TestUtility.checkExtractReferences("Nerv_BVI", documentAnalyser, expectedDocumentType, resourceFolder, numberOfSystem = 1, numberOfSubSystem = 1)
  }

  "BSVDocumentEnricher" should "be able to extract modules from Nerv" in {
    TestUtility.checkExtractReferences("Nerv", documentAnalyser, expectedDocumentType, resourceFolder, numberOfSystem = 1, numberOfSubSystem = 1)
  }
}



