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

  def copy(
            documentName: String = documentName,
            filePath: String = filePath,
            references: Set[DocReference] = references,
            relations: Set[DocRelation] = relations,
            events: Set[DocReference] = events,
            requirements: Set[DocReference] = requirements,
            scenarios: Set[DocReference] = scenarios,
            documentType: DocumentType = documentType,
          ): LandoDocumentInfo = {
    new LandoDocumentInfo(
      documentName,
      filePath,
      references,
      relations,
      events,
      requirements,
      scenarios,
      documentType,
    )
  }

  private val fileUtil = new FileUtil()
  private val validReferenceTypesTypes: Set[ReferenceType] = Set(ReferenceType.Event, ReferenceType.Scenario, ReferenceType.Requirement, ReferenceType.System, ReferenceType.SubSystem, ReferenceType.Component)

  require(getAllReferences.forall(ref => validReferenceTypesTypes.contains(ref.getReferenceType)
    && ref.getDocumentName == documentName
    && ref.getDocumentType == DocumentType.Lando))
  require(events.forall(_.getReferenceType == ReferenceType.Event), "All events must be of type Event")
  require(scenarios.forall(_.getReferenceType == ReferenceType.Scenario), "All scenarios must be of type Scenario")
  require(requirements.forall(_.getReferenceType == ReferenceType.Requirement), "All requirements must be of type Requirement")
  //require(references.forall(_.getReferenceType ))

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







