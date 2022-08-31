import DocumentEnrichers.SysMLDocumentEnricher
import Types.{ReferenceType, DocumentType}
import Utils.{Control, FileUtil}
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import java.io.File
import scala.collection.mutable
import scala.io.Source

class SysMLAnalyzerTest extends AnyFlatSpec with should.Matchers {
  private val fileUtil = FileUtil()
  private val documentEnricher = SysMLDocumentEnricher()

  "SysMLReader" should "to extract parts and items" in {
    checkExtractReferences("RTS_Glossary", 1, 80, 66, 0, 0, 0, 0, 0)
  }

  "SysMLReader" should "to extract requirements" in {
    checkExtractReferences("RTS_Requirements", 4, 0, 0, 17, 0, 0, 0, 0)
  }

  "SysMLReader" should "to extract packages" in {
    checkExtractReferences("HARDENS", 7, 0, 0, 0, 0, 0, 0, 0)
  }

  "SysMLReader" should "to extract Scenarios" in {
    checkExtractReferences("RTS_Scenarios", 4, 0, 1, 0, 39, 0, 0, 0)
  }


  "SysMLReader" should "to extract Views" in {
    checkExtractReferences("RTS_Viewpoints", 1, 0, 0, 0, 0, 8, 0, 0)
  }


  "SysMLReader" should "to extract Actions" in {
    checkExtractReferences("RTS_Actions", 4, 0, 0, 0, 0, 0, 20, 0)
  }

  private def checkExtractReferences(fileName: String,
                                     numberOfSystem: Int,
                                     numberOfSubSystem: Int,
                                     numberOfComponent: Int,
                                     numberOfRequirements: Int,
                                     numberOfScenarios: Int,
                                     numberOfViews: Int,
                                     numberOfEvents: Int,
                                     numberOfAttributes: Int
                                    ) = {
    val sysmlDocuments = getClass.getResource("SysML").getPath
    val filesToAnalyze = fileUtil.getListOfFiles(sysmlDocuments).toArray

    val documentOfInterest = filesToAnalyze.filter(_.contains(fileName))
    assert(documentOfInterest.length == 1)
    val filePath = documentOfInterest.head

    val analyzedDocument = documentEnricher.extractDocumentInfo(filePath)
    assert(analyzedDocument.documentName == fileName)
    assert(analyzedDocument.documentType == DocumentType.SysML)

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



