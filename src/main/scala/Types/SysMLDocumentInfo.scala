package Types

import DocumentEnrichers.FileUtil

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

  private val validRefenceTypesTypes: Set[ReferenceType] = Set(ReferenceType.Scenario, ReferenceType.Requirement, ReferenceType.Event, ReferenceType.Action, ReferenceType.Package, ReferenceType.UseCase, ReferenceType.Part, ReferenceType.Connection, ReferenceType.Import, ReferenceType.View, ReferenceType.ViewPoint, ReferenceType.Item)

  require(getAllReferences.forall(ref => validRefenceTypesTypes.contains(ref.referenceType) && ref.documentType == DocumentType.SysML && ref.documentName == documentName))
  require(parts.forall(_.referenceType == ReferenceType.Part))
  require(connections.forall(_.referenceType == ReferenceType.Connection))
  require(packages.forall(_.referenceType == ReferenceType.Package))
  require(actions.forall(_.referenceType == ReferenceType.Action))
  require(views.forall(_.referenceType == ReferenceType.View))
  require(imports.forall(_.referenceType == ReferenceType.Import))
  require(usecases.forall(_.referenceType == ReferenceType.UseCase))
  require(items.forall(_.referenceType == ReferenceType.Item))

  //All referenceNames must be unique

  require(requirements.forall(_.referenceType == ReferenceType.Requirement))

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
