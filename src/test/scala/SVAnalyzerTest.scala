import DocumentEnrichers.SVDocumentEnricher
import Types.{DocumentType, ReferenceType}
import Utils.{Control, FileUtil}
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import java.io.File
import scala.collection.mutable
import scala.io.Source

class SVAnalyzerTest extends AnyFlatSpec with should.Matchers {
  private val fileUtil = FileUtil()
  private val documentAnalyser = SVDocumentEnricher()

  "SVDocumentEnricher" should "be able to extract modules from AcutationUnit" in {
    checkExtractReferences("actuation_unit_impl", numberOfSystem = 7)
  }

  "SVDocumentEnricher" should "be able to extract modules from Actuator Impl" in {
    checkExtractReferences("actuator_impl", numberOfSystem = 1)
  }

  "SVDocumentEnricher" should "be able to extract modules from Instrumentation Impl Generated" in {
    checkExtractReferences("instrumentation_impl", numberOfSystem = 3)
  }

  "SVDocumentEnricher" should "be able to extract modules from Instrumentation Impl Handwritten" in {
    checkExtractReferences("instrumentation_impl_handwritten", numberOfSystem = 2)
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
    val systemVerilogDocuments = getClass.getResource("SystemVerilog").getPath
    val filesToAnalyze = fileUtil.getListOfFiles(systemVerilogDocuments).toArray

    val documentOfInterest = filesToAnalyze.filter(_.contains(fileName))
    assert(documentOfInterest.length == 1)
    val filePath = documentOfInterest.head

    val analyzedDocument = documentAnalyser.extractDocumentInfo(filePath)
    assert(analyzedDocument.documentName == fileName)
    assert(analyzedDocument.documentType == DocumentType.SV)

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



