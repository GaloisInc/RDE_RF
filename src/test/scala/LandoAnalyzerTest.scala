import DocumentEnrichers.LandoDocumentEnricher
import Types.{ReferenceType, DocumentType}
import Utils.{Control, FileUtil}
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import java.io.File
import scala.collection.mutable
import scala.io.Source

class LandoAnalyzerTest extends AnyFlatSpec with should.Matchers {
  private val fileUtil = FileUtil()
  private val landoDocumentEnricher = LandoDocumentEnricher()

  "LandoDocumentEnricher" should "be able to extract glossary" in {
    checkExtractReferences("glossary", 0,1,104, 0, 0,0,0,0)
  }

  "LandoDocumentEnricher" should "be able to extract system" in {
    checkExtractReferences("RTS", 1,6,0, 0, 0,0,0,0)
  }

  "LandoDocumentEnricher" should "be able to extract events" in {
    checkExtractReferences("events", 0,0,0, 0, 0,16,0,0)
  }

  "LandoDocumentEnricher" should "be able to extract requirements" in {
    checkExtractReferences("project_requirements", 0,0,0, 16, 0,0,0,0)
  }

  "LandoDocumentEnricher" should "be able to extract scenarios" in {
    checkExtractReferences("test_scenarios", 0,0,0, 0, 40,0,0,0)
  }

  private def checkExtractReferences(fileName: String,
                                     numberOfSystem: Int,
                                     numberOfSubSystem: Int,
                                     numberOfComponent: Int,
                                     numberOfRequirements: Int,
                                     numberOfScenarios: Int,
                                     numberOfEvents: Int,
                                     numberOfAttributes: Int = 0,
                                     numberOfViews: Int = 0,
                                    ) = {
    val landoDocuments = getClass.getResource("Lando").getPath
    val filesToAnalyze = fileUtil.getListOfFiles(landoDocuments).toArray

    val documentOfInterest = filesToAnalyze.filter(_.contains(fileName))
    assert(documentOfInterest.length == 1)
    val filePath = documentOfInterest.head

    val analyzedDocument = landoDocumentEnricher.extractDocumentInfo(filePath)
    assert(analyzedDocument.documentName == fileName)
    assert(analyzedDocument.documentType == DocumentType.Lando)

    val extractedReferences = analyzedDocument.getAllReferences
    assert(extractedReferences.count(_.referenceType == ReferenceType.System) == numberOfSystem)
    assert(extractedReferences.count(_.referenceType == ReferenceType.SubSystem) == numberOfSubSystem)
    assert(extractedReferences.count(_.referenceType == ReferenceType.Component) == numberOfComponent)
    assert(extractedReferences.count(_.referenceType == ReferenceType.Requirement) == numberOfRequirements)
    assert(extractedReferences.count(_.referenceType == ReferenceType.Scenario) == numberOfScenarios)
    assert(extractedReferences.count(_.referenceType == ReferenceType.View) == numberOfViews)
    assert(extractedReferences.count(_.referenceType == ReferenceType.Event) == numberOfEvents)
    assert(extractedReferences.count(_.referenceType == ReferenceType.Attribute) == numberOfAttributes)
  }
}



