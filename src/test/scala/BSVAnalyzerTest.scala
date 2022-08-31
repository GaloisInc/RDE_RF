import DocumentEnrichers.BSVDocumentEnricher
import Types.{DocumentType, ReferenceType}
import Utils.{Control, FileUtil}
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import java.io.File
import scala.collection.mutable
import scala.io.Source

class BSVAnalyzerTest extends AnyFlatSpec with should.Matchers {
  private val fileUtil = FileUtil()
  private val documentAnalyser = BSVDocumentEnricher()

  "BSVDocumentEnricher" should "be able to extract modules from Actuation" in {
    checkExtractReferences("Actuation", numberOfSystem = 1)
  }

  "BSVDocumentEnricher" should "be able to extract modules from Actuation_Generated_BVI" in {
    checkExtractReferences("Actuation_Generated_BVI", numberOfSystem = 1, numberOfSubSystem = 3)
  }

  "BSVDocumentEnricher" should "be able to extract modules from Instrumentation_Handwritten_BVI" in {
    checkExtractReferences("Instrumentation_Handwritten_BVI", numberOfSystem = 1, numberOfSubSystem = 3)
  }

  "BSVDocumentEnricher" should "be able to extract modules from Instrumentation" in {
    checkExtractReferences("Instrumentation", numberOfSystem = 1)
  }

  "BSVDocumentEnricher" should "be able to extract modules from Nerv_BVI" in {
    checkExtractReferences("Nerv_BVI", numberOfSystem = 1, numberOfSubSystem = 1)
  }

  "BSVDocumentEnricher" should "be able to extract modules from Nerv" in {
    checkExtractReferences("Nerv", numberOfSystem = 1, numberOfSubSystem = 1)
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

    val systemVerilogDocuments = getClass.getResource("BSV").getPath
    val filesToAnalyze = fileUtil.getListOfFiles(systemVerilogDocuments).toArray

    val documentOfInterest = filesToAnalyze.filter(path => fileUtil.getFileName(path) == fileName)
    assert(documentOfInterest.length == 1)
    val filePath = documentOfInterest.head

    val analyzedDocument = documentAnalyser.extractDocumentInfo(filePath)
    assert(analyzedDocument.documentName == fileName)
    assert(analyzedDocument.documentType == DocumentType.BSV)

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
  }
}



