package Types.DocumentInfos

import Types.*
import Utils.FileUtil

class SysMLDocumentInfo(
                         override val documentName: String,
                         override val filePath: String,
                         packages: Set[DocReference],
                         parts: Set[DocReference],
                         connections: Set[DocReference],
                         usecases: Set[DocReference],
                         requirements: Set[DocReference],
                         actions: Set[DocReference],
                         imports: Set[DocReference],
                         views: Set[DocReference],
                         items: Set[DocReference],
                         override val documentType: DocumentType = DocumentType.SysML
                       ) extends DocumentInfo {
  val fileUtil = new FileUtil()

  def copy(
            documentName: String = documentName,
            filePath: String = filePath,
            packages: Set[DocReference] = packages,
            parts: Set[DocReference] = parts,
            connections: Set[DocReference] = connections,
            usecases: Set[DocReference] = usecases,
            requirements: Set[DocReference] = requirements,
            actions: Set[DocReference] = actions,
            imports: Set[DocReference] = imports,
            views: Set[DocReference] = views,
            items: Set[DocReference] = items,
            documentType: DocumentType = documentType
          ): SysMLDocumentInfo = {
    new SysMLDocumentInfo(
      documentName,
      filePath,
      packages,
      parts,
      connections,
      usecases,
      requirements,
      actions,
      imports,
      views,
      items,
      documentType
    )
  }

  private val validRefenceTypesTypes: Set[ReferenceType] = Set(ReferenceType.Scenario, ReferenceType.Requirement, ReferenceType.Event, ReferenceType.System, ReferenceType.Scenario, ReferenceType.SubSystem, ReferenceType.Connection, ReferenceType.Import, ReferenceType.View, ReferenceType.ViewPoint, ReferenceType.Component)

  require(getAllReferences.forall(ref => validRefenceTypesTypes.contains(ref.getReferenceType) && ref.getDocumentType == DocumentType.SysML && ref.getDocumentName == documentName))
  require(parts.forall(_.getReferenceType == ReferenceType.SubSystem))
  require(connections.forall(_.getReferenceType == ReferenceType.Connection))
  require(packages.forall(_.getReferenceType == ReferenceType.System))
  require(actions.forall(_.getReferenceType == ReferenceType.Event))
  require(views.forall(_.getReferenceType == ReferenceType.View))
  require(imports.forall(_.getReferenceType == ReferenceType.Import))
  require(usecases.forall(_.getReferenceType == ReferenceType.Scenario))
  require(items.forall(_.getReferenceType == ReferenceType.Component))

  //All referenceNames must be unique

  require(requirements.forall(_.getReferenceType == ReferenceType.Requirement))

  override lazy val getAllReferences: Set[DocReference] = {
    packages ++ parts ++ connections ++ usecases ++ requirements ++ actions ++ imports ++ views ++ items
  }

  override def getRelations: Set[DocRelation] = Set.empty

  override def getFileType: FileType = {
    if (fileUtil.isFileType(filePath, "action")) FileType.EventFile
    else if (fileUtil.isFileType(filePath, "requirement")) FileType.RequirementFile
    else if (fileUtil.isFileType(filePath, "use case")) FileType.ScenarioFile
    else if (fileUtil.isFileType(filePath, "view")) FileType.ViewFile
    else FileType.ComponentFile
  }
}
