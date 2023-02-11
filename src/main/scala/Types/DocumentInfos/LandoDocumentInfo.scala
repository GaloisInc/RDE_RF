package Types.DocumentInfos

import Types.DocReference.DocReference
import Types.{DocRelation, DocumentType, FileType, ReferenceType}
import Utils.FileUtil

class LandoDocumentInfo(
                         override val documentName: String,
                         override val filePath: String,
                         references: Set[DocReference],
                         relations: Set[DocRelation],
                         events: Set[DocReference],
                         requirements: Set[DocReference],
                         scenarios: Set[DocReference],
                       ) extends DocumentInfo[LandoDocumentInfo] {

  override val documentType: DocumentType.Value = DocumentType.Lando
  val validReferenceTypesTypes: Set[ReferenceType.Value] = Set(ReferenceType.Event, ReferenceType.Scenario, ReferenceType.Requirement, ReferenceType.System, ReferenceType.SubSystem, ReferenceType.Component)
  val latexLanguageName = "Lando"

  def copy(
            documentName: String = documentName,
            filePath: String = filePath,
            references: Set[DocReference] = references,
            relations: Set[DocRelation] = relations,
            events: Set[DocReference] = events,
            requirements: Set[DocReference] = requirements,
            scenarios: Set[DocReference] = scenarios,
          ): LandoDocumentInfo = {
    new LandoDocumentInfo(
      documentName,
      filePath,
      references,
      relations,
      events,
      requirements,
      scenarios)
  }

  override def updateReference(ref: DocReference): LandoDocumentInfo = {
    ref.getReferenceType match {
      case Types.ReferenceType.Component | Types.ReferenceType.System | Types.ReferenceType.SubSystem =>
        val newReferences = references.filterNot(_.getName.equalsIgnoreCase(ref.getName)) + ref
        copy(references = newReferences)
      case Types.ReferenceType.Scenario =>
        val newScenarios = scenarios.filterNot(_.getName.equalsIgnoreCase(ref.getName)) + ref
        copy(scenarios = newScenarios)
      case Types.ReferenceType.Requirement =>
        val newRequirements = requirements.filterNot(_.getName.equalsIgnoreCase(ref.getName)) + ref
        copy(requirements = newRequirements)
      case Types.ReferenceType.Event =>
        val newEvents = events.filterNot(_.getName.equalsIgnoreCase(ref.getName)) + ref
        copy(events = newEvents)
      case _ => throw new Exception("Unknown reference type")
    }
  }

  override def updateFilePath(newFilePath: String): LandoDocumentInfo = {
    copy(filePath = newFilePath)
  }

  require(events.forall(_.getReferenceType == ReferenceType.Event), "All events must be of type Event")
  require(scenarios.forall(_.getReferenceType == ReferenceType.Scenario), "All scenarios must be of type Scenario")
  require(requirements.forall(_.getReferenceType == ReferenceType.Requirement), "All requirements must be of type Requirement")

  override lazy val getAllReferences: Set[DocReference] = {
    references ++ events ++ requirements ++ scenarios
  }

  override lazy val getRelations: Set[DocRelation] = relations

  override def getFileType: FileType.Value = {
    if (FileUtil.isOfFileType(filePath, "events")) FileType.EventFile
    else if (FileUtil.isOfFileType(filePath, "requirements")) FileType.RequirementFile
    else if (FileUtil.isOfFileType(filePath, "scenarios")) FileType.ScenarioFile
    else FileType.ComponentFile
  }
}












