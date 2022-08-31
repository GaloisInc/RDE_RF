package Types.DocumentInfos

import Types.*
import Utils.FileUtil

class LandoDocumentInfo(
                         override val documentName: String,
                         override val filePath: String,
                         references: Set[DocReference],
                         relations: Set[DocRelation],
                         events: Set[DocReference],
                         requirements: Set[DocReference],
                         scenarios: Set[DocReference],
                         override val documentType: DocumentType = DocumentType.Lando,
                       ) extends DocumentInfo {

  private val fileUtil = new FileUtil()
  private val validReferenceTypesTypes: Set[ReferenceType] = Set(ReferenceType.Event, ReferenceType.Scenario, ReferenceType.Requirement, ReferenceType.System, ReferenceType.SubSystem, ReferenceType.Component)

  require(getAllReferences.forall(ref => validReferenceTypesTypes.contains(ref.referenceType)
    && ref.documentName == documentName
    && ref.documentType == DocumentType.Lando))
  require(events.forall(_.referenceType == ReferenceType.Event))
  require(scenarios.forall(_.referenceType == ReferenceType.Scenario))
  require(requirements.forall(_.referenceType == ReferenceType.Requirement))
  //require(references.forall(_.referenceType ))

  override lazy val getAllReferences: Set[DocReference] = {
    references ++ events ++ requirements ++ scenarios
  }

  lazy val getRelations: Set[DocRelation] = relations

  override def getFileType: FileType = {
    if (fileUtil.isFileType(filePath, "events")) FileType.EventFile
    else if (fileUtil.isFileType(filePath, "requirements")) FileType.RequirementFile
    else if (fileUtil.isFileType(filePath, "scenarios")) FileType.ScenarioFile
    FileType.ComponentFile
  }
}







