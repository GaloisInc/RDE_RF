import DocumentEnrichers.LandoDocumentEnricher
import Types.{ReferenceType, DocumentType}
import Utils.{Control, FileUtil}
import Formatter.InlineFormatter
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import java.io.File
import scala.collection.mutable
import scala.io.Source

class LandoAnalyzerTest extends AnyFlatSpec with should.Matchers {
  private val fileUtil = FileUtil()
  private val formatterType = InlineFormatter()
  private val landoDocumentEnricher = LandoDocumentEnricher(formatterType)
  private val expectedDocumentType = DocumentType.Lando
  private val resourceFolder = "lando"

  "LandoDocumentEnricher" should "be able to extract glossary" in {
    TestUtility.checkExtractReferences("glossary", landoDocumentEnricher, expectedDocumentType, resourceFolder, 0, 1, 104, 0, 0, 0, 0, 0)
  }

  "LandoDocumentEnricher" should "be able to extract system" in {
    TestUtility.checkExtractReferences("RTS", landoDocumentEnricher, expectedDocumentType, resourceFolder, 1, 6, 0, 0, 0, 0, 0, 0)
  }

  "LandoDocumentEnricher" should "be able to extract events" in {
    TestUtility.checkExtractReferences("events", landoDocumentEnricher, expectedDocumentType, resourceFolder, 0, 0, 0, 0, 0, 16, 0, 0)
  }

  "LandoDocumentEnricher" should "be able to extract requirements" in {
    TestUtility.checkExtractReferences("project_requirements", landoDocumentEnricher, expectedDocumentType, resourceFolder, 0, 0, 0, 16, 0, 0, 0, 0)
  }

  "LandoDocumentEnricher" should "be able to extract scenarios" in {
    TestUtility.checkExtractReferences("test_scenarios", landoDocumentEnricher, expectedDocumentType, resourceFolder, 0, 0, 0, 0, 40, 0, 0, 0)
  }

}



