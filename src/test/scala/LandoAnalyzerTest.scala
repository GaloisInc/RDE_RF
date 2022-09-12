import DocumentEnrichers.LandoDocumentEnricher
import Formatter.InlineFormatter
import TestUtils.TestUtility
import Types.{DocumentType, ReferenceName, ReferenceType}
import Utils.{Control, FileUtil}
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import java.io.File
import scala.collection.mutable
import scala.io.Source

class LandoAnalyzerTest extends AnyFlatSpec with should.Matchers {
  private val formatterType = InlineFormatter()
  private val landoDocumentEnricher = LandoDocumentEnricher(formatterType)
  private val expectedDocumentType = DocumentType.Lando
  private val resourceFolder = "../lando"
  private val testUtility = TestUtility()

  "LandoDocumentEnricher" should "be able to extract glossary" in {
    val fileName = "glossary"
    testUtility.checkExtractReferences(fileName, landoDocumentEnricher, expectedDocumentType, resourceFolder, 0, 1, 104)
  }

  "LandoDocumentEnricher" should "be able to extract system" in {
    val fileName = "RTS"
    testUtility.checkExtractReferences(fileName, landoDocumentEnricher, expectedDocumentType, resourceFolder, 1, 6)
  }

  "LandoDocumentEnricher" should "be able to extract events" in {
    val fileName = "events"
    testUtility.checkExtractReferences(fileName, landoDocumentEnricher, expectedDocumentType, resourceFolder, numberOfEvents = 16)
  }

  "LandoDocumentEnricher" should "be able to extract requirements" in {
    val fileName = "project_requirements"
    testUtility.checkExtractReferences(fileName, landoDocumentEnricher, expectedDocumentType, resourceFolder, numberOfRequirements = 13)
  }

  "LandoDocumentEnricher" should "be able to extract scenarios" in {
    val fileName = "test_scenarios"
    testUtility.checkExtractReferences(fileName, landoDocumentEnricher, expectedDocumentType, resourceFolder, numberOfScenarios = 40)
  }

  it should "be able to extract referenceName" in {
    landoDocumentEnricher.extractReferenceName("component RTS (RTS)") should be (ReferenceName("RTS", Some("RTS")))
    landoDocumentEnricher.extractReferenceName("system Real Time System (RTS)") should be (ReferenceName("Real Time System", Some("RTS")))
    landoDocumentEnricher.extractReferenceName("subsystem Event 1 (E1)") should be (ReferenceName("Event 1", Some("E1")))
    landoDocumentEnricher.extractReferenceName("subsystem Event 1") should be (ReferenceName("Event 1", None))
  }
  
}



