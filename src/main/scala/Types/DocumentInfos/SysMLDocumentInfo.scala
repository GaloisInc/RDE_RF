package Types.DocumentInfos

import Types.{DocRelation, DocumentType, FileType, ReferenceType}
import Types.DocReference.DocReference
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
                         attributes: Set[DocReference]) extends DocumentInfo[SysMLDocumentInfo] {

  override val documentType: DocumentType.Value = DocumentType.SysML

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
            attributes: Set[DocReference] = attributes
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
      attributes)
  }

  private val validRefenceTypesTypes: Set[ReferenceType.Value] = Set(ReferenceType.Scenario, ReferenceType.Requirement, ReferenceType.Event, ReferenceType.System, ReferenceType.Scenario, ReferenceType.SubSystem, ReferenceType.Connection, ReferenceType.Import, ReferenceType.View, ReferenceType.ViewPoint, ReferenceType.Component, ReferenceType.Attribute)

  require(getAllReferences.forall(ref => validRefenceTypesTypes.contains(ref.getReferenceType) && ref.getDocumentType == DocumentType.SysML && ref.getDocumentName == documentName))
  require(parts.forall(_.getReferenceType == ReferenceType.SubSystem))
  require(connections.forall(_.getReferenceType == ReferenceType.Connection))
  require(packages.forall(_.getReferenceType == ReferenceType.System))
  require(actions.forall(_.getReferenceType == ReferenceType.Event))
  require(views.forall(_.getReferenceType == ReferenceType.View))
  require(imports.forall(_.getReferenceType == ReferenceType.Import))
  require(usecases.forall(_.getReferenceType == ReferenceType.Scenario))
  require(items.forall(_.getReferenceType == ReferenceType.Component))
  require(attributes.forall(_.getReferenceType == ReferenceType.Attribute))

  //All referenceNames must be unique

  require(requirements.forall(_.getReferenceType == ReferenceType.Requirement))

  override def updateReference(ref: DocReference): SysMLDocumentInfo = {
    ref.getReferenceType match {
      case ReferenceType.Import => copy(imports = imports.map(_.updateDocReference(ref)))
      case ReferenceType.System => copy(packages = packages.map(_.updateDocReference(ref)))
      case ReferenceType.SubSystem => copy(parts = parts.map(_.updateDocReference(ref)))
      case ReferenceType.Connection => copy(connections = connections.map(_.updateDocReference(ref)))
      case ReferenceType.Scenario => copy(usecases = usecases.map(_.updateDocReference(ref)))
      case ReferenceType.Requirement => copy(requirements = requirements.map(_.updateDocReference(ref)))
      case ReferenceType.Event => copy(actions = actions.map(_.updateDocReference(ref)))
      case ReferenceType.View => copy(views = views.map(_.updateDocReference(ref)))
      case ReferenceType.Component => copy(items = items.map(_.updateDocReference(ref)))
      case ReferenceType.Attribute => copy(attributes = attributes.map(_.updateDocReference(ref)))
      case _ => throw new IllegalArgumentException("Invalid reference type")
    }
  }

  override def updateFilePath(newFilePath: String): SysMLDocumentInfo = {
    copy(filePath = newFilePath)
  }

  override lazy val getAllReferences: Set[DocReference] = {
    packages ++ parts ++ connections ++ usecases ++ requirements ++ actions ++ imports ++ views ++ items ++ attributes
  }

  override def getRelations: Set[DocRelation] = Set.empty

  override def getFileType: FileType.Value = {
    if (FileUtil.isOfFileType(filePath, "action")) FileType.EventFile
    else if (FileUtil.isOfFileType(filePath, "requirement")) FileType.RequirementFile
    else if (FileUtil.isOfFileType(filePath, "use case")) FileType.ScenarioFile
    else if (FileUtil.isOfFileType(filePath, "view")) FileType.ViewFile
    else FileType.ComponentFile
  }
}
