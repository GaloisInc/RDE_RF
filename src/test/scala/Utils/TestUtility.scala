package TestUtils

import DocumentEnrichers.DocumentEnricher
import Types.{DocumentType, ReferenceType}
import Utils.FileUtil

class TestUtility {
  def checkExtractReferences(fileName: String,
                             documentEnricher: DocumentEnricher,
                             expectedDocumentType: DocumentType,
                             filePath: String,
                             numberExprectedSystem: Int = 0,
                             numberOfSubSystem: Int = 0,
                             numberOfComponent: Int = 0,
                             numberOfRequirements: Int = 0,
                             numberOfScenarios: Int = 0,
                             numberOfEvents: Int = 0,
                             numberOfAttributes: Int = 0,
                             numberOfViews: Int = 0,
                             numberOfTypes: Int = 0
                            ): Boolean = {
    val analyzedDocument = documentEnricher.extractDocumentInfo(filePath)
    assert(analyzedDocument.documentName == fileName, "Document name is not correct")
    assert(analyzedDocument.documentType == expectedDocumentType, "Document type is not correct")

    val extractedReferences = analyzedDocument.getAllReferences
    assert(extractedReferences.count(_.getReferenceType == ReferenceType.System) == numberExprectedSystem, "Number of system references is not correct")
    assert(extractedReferences.count(_.getReferenceType == ReferenceType.SubSystem) == numberOfSubSystem, "Number of subsystem references is not correct")
    assert(extractedReferences.count(_.getReferenceType == ReferenceType.Component) == numberOfComponent, "Number of component references is not correct")
    assert(extractedReferences.count(_.getReferenceType == ReferenceType.Requirement) == numberOfRequirements, "Number of requirement references is not correct")
    assert(extractedReferences.count(_.getReferenceType == ReferenceType.Attribute) == numberOfAttributes, "Number of attribute references is not correct")
    assert(extractedReferences.count(_.getReferenceType == ReferenceType.View) == numberOfViews, "Number of view references is not correct")
    assert(extractedReferences.count(_.getReferenceType == ReferenceType.Event) == numberOfEvents, "Number of event references is not correct")
    assert(extractedReferences.count(_.getReferenceType == ReferenceType.Attribute) == numberOfAttributes, "Number of attribute references is not correct")
    assert(extractedReferences.count(_.getReferenceType == ReferenceType.Type) == numberOfTypes, "Number of type references is not correct")
    assert(extractedReferences.count(_.getReferenceType == ReferenceType.Scenario) == numberOfScenarios, "Number of scenario references is not correct")
    true
  }

}
