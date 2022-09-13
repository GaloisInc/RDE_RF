package TestUtils

import DocumentEnrichers.DocumentEnricher
import Types.{DocumentType, ReferenceType}
import Utils.FileUtil

class TestUtility {
  def checkExtractReferences(fileName: String,
                             documentEnricher: DocumentEnricher,
                             expectedDocumentType: DocumentType,
                             resourceFolderName: String,
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

    val documents = getClass.getResource(resourceFolderName).getPath
    val filesToAnalyze = FileUtil.getListOfFiles(documents).toArray
    val documentOfInterest = filesToAnalyze.filter(path => FileUtil.getFileName(path) == fileName)

    assert(documentOfInterest.length == 1)
    val filePath = documentOfInterest.head

    val analyzedDocument = documentEnricher.parseDocument(filePath)
    assert(analyzedDocument.documentName == fileName, "Document name is not correct")
    assert(analyzedDocument.documentType == expectedDocumentType, "Document type is not correct")

    val extractedReferences = analyzedDocument.getAllReferences
    assert(extractedReferences.count(_.getReferenceType == ReferenceType.System) == numberExprectedSystem, "Number of system references is not correct. Expected: " + numberExprectedSystem + " Actual: " + extractedReferences.count(_.getReferenceType == ReferenceType.System))
    assert(extractedReferences.count(_.getReferenceType == ReferenceType.SubSystem) == numberOfSubSystem, "Number of subsystem references is not correct. Expected: " + numberOfSubSystem + " Actual: " + extractedReferences.count(_.getReferenceType == ReferenceType.SubSystem))
    assert(extractedReferences.count(_.getReferenceType == ReferenceType.Component) == numberOfComponent, "Number of component references is not correct. Expected: " + numberOfComponent + " Actual: " + extractedReferences.count(_.getReferenceType == ReferenceType.Component))
    assert(extractedReferences.count(_.getReferenceType == ReferenceType.Requirement) == numberOfRequirements, "Number of requirement references is not correct. Expected: " + numberOfRequirements + " Actual: " + extractedReferences.count(_.getReferenceType == ReferenceType.Requirement))
    assert(extractedReferences.count(_.getReferenceType == ReferenceType.Attribute) == numberOfAttributes, "Number of attribute references is not correct. Expected: " + numberOfAttributes + " Actual: " + extractedReferences.count(_.getReferenceType == ReferenceType.Attribute))
    assert(extractedReferences.count(_.getReferenceType == ReferenceType.View) == numberOfViews, "Number of view references is not correct. Expected: " + numberOfViews + " Actual: " + extractedReferences.count(_.getReferenceType == ReferenceType.View))
    assert(extractedReferences.count(_.getReferenceType == ReferenceType.Event) == numberOfEvents, "Number of event references is not correct. Expected: " + numberOfEvents + " Actual: " + extractedReferences.count(_.getReferenceType == ReferenceType.Event))
    assert(extractedReferences.count(_.getReferenceType == ReferenceType.Attribute) == numberOfAttributes, "Number of attribute references is not correct. Expected: " + numberOfAttributes + " Actual: " + extractedReferences.count(_.getReferenceType == ReferenceType.Attribute))
    assert(extractedReferences.count(_.getReferenceType == ReferenceType.Type) == numberOfTypes, "Number of type references is not correct. Expected: " + numberOfTypes + " Actual: " + extractedReferences.count(_.getReferenceType == ReferenceType.Type))
    assert(extractedReferences.count(_.getReferenceType == ReferenceType.Scenario) == numberOfScenarios, "Number of scenario references is not correct. Expected: " + numberOfScenarios + " Actual: " + extractedReferences.count(_.getReferenceType == ReferenceType.Scenario))
    true
  }

}
