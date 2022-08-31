import DocumentEnrichers.CryptolDocumentEnricher
import Types.{ReferenceType, DocumentType}
import Utils.{Control, FileUtil}
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import java.io.File
import scala.collection.mutable
import scala.io.Source

class CryptolAnalyzerTest extends AnyFlatSpec with should.Matchers {
  private val fileUtil = FileUtil()
  private val documentAnalyser = CryptolDocumentEnricher()

  "CryptolDocumentEnricher" should "be able to extract types from AcutationUnit" in {
    checkExtractReferences("ActuationUnit", numberOfTypes = 5, numberOfRequirements = 5, numberOfEvents = 9)
  }

  "CryptolDocumentEnricher" should "be able to extract types from Actuator" in {
    checkExtractReferences("Actuator", numberOfTypes = 3, numberOfEvents = 3)
  }

  "CryptolDocumentEnricher" should "be able to extract types from InstrumentationUnit" in {
    checkExtractReferences("InstrumentationUnit", numberOfTypes = 10, numberOfRequirements = 9, numberOfEvents = 9)
  }

  "CryptolDocumentEnricher" should "be able to extract types from RTS" in {
    checkExtractReferences("RTS", numberOfTypes = 10, numberOfRequirements = 6, numberOfEvents = 7)
  }

  "CryptolDocumentEnricher" should "be able to extract types from Utils" in {
    checkExtractReferences("Utils", numberOfEvents = 1)
  }


  private def checkExtractReferences(fileName: String,
                                     numberOfSystem: Int = 0,
                                     numberOfSubSystem: Int = 0,
                                     numberOfComponent: Int = 0,
                                     numberOfRequirements: Int = 0,
                                     numberOfScenarios: Int = 0,
                                     numberOfEvents: Int = 0,
                                     numberOfAttributes: Int = 0,
                                     numberOfViews: Int = 0,
                                     numberOfTypes: Int = 0
                                    ) = {
    val cryptolDocuments = getClass.getResource("Cryptol").getPath
    val filesToAnalyze = fileUtil.getListOfFiles(cryptolDocuments).toArray

    val documentOfInterest = filesToAnalyze.filter(_.contains(fileName))
    assert(documentOfInterest.length == 1)
    val filePath = documentOfInterest.head

    val analyzedDocument = documentAnalyser.extractDocumentInfo(filePath)
    assert(analyzedDocument.documentName == fileName)
    assert(analyzedDocument.documentType == DocumentType.Cryptol)

    val extractedReferences = analyzedDocument.getAllReferences
    assert(extractedReferences.count(_.referenceType == ReferenceType.System) == numberOfSystem)
    assert(extractedReferences.count(_.referenceType == ReferenceType.SubSystem) == numberOfSubSystem)
    assert(extractedReferences.count(_.referenceType == ReferenceType.Component) == numberOfComponent)
    assert(extractedReferences.count(_.referenceType == ReferenceType.Requirement) == numberOfRequirements)
    assert(extractedReferences.count(_.referenceType == ReferenceType.Attribute) == numberOfAttributes)
    assert(extractedReferences.count(_.referenceType == ReferenceType.View) == numberOfViews)
    assert(extractedReferences.count(_.referenceType == ReferenceType.Event) == numberOfEvents)
    assert(extractedReferences.count(_.referenceType == ReferenceType.Attribute) == numberOfAttributes)
    assert(extractedReferences.count(_.referenceType == ReferenceType.Type) == numberOfTypes)
    assert(extractedReferences.count(_.referenceType == ReferenceType.Scenario) == numberOfScenarios)


  }
}



