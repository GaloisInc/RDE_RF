package Types.DocumentInfos

import Types.{DocRelation, DocumentType, FileType, ReferenceType}
import Types.DocReference.DocReference
import Utils.FileUtil

case class SysMLDocumentInfo(
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
  val validReferenceTypesTypes: Set[ReferenceType.Value] = Set(ReferenceType.Scenario, ReferenceType.Requirement, ReferenceType.Event, ReferenceType.System, ReferenceType.Scenario, ReferenceType.SubSystem, ReferenceType.Connection, ReferenceType.Import, ReferenceType.View, ReferenceType.ViewPoint, ReferenceType.Component, ReferenceType.Attribute)
  override val latexLanguageName = "SysML"

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
      case ReferenceType.Import => copy(imports = imports.filterNot(_.originalLine == ref.originalLine) + ref)
      case ReferenceType.System => copy(packages = packages.filterNot(_.originalLine == ref.originalLine) + ref)
      case ReferenceType.SubSystem => copy(parts = parts.filterNot(_.originalLine == ref.originalLine) + ref)
      case ReferenceType.Connection => copy(connections = connections.filterNot(_.originalLine == ref.originalLine) + ref)
      case ReferenceType.Scenario => copy(usecases = usecases.filterNot(_.originalLine == ref.originalLine) + ref)
      case ReferenceType.Requirement => copy(requirements = requirements.filterNot(_.originalLine == ref.originalLine) + ref)
      case ReferenceType.Event => copy(actions = actions.filterNot(_.originalLine == ref.originalLine) + ref)
      case ReferenceType.View => copy(views = views.filterNot(_.originalLine == ref.originalLine) + ref)
      case ReferenceType.Component => copy(items = items.filterNot(_.originalLine == ref.originalLine) + ref)
      case ReferenceType.Attribute => copy(attributes = attributes.filterNot(_.originalLine == ref.originalLine) + ref)
      case _ => throw new IllegalArgumentException("Invalid reference type")
    }
  } ensuring((newDoc: SysMLDocumentInfo) => newDoc.getAllReferences.size == getAllReferences.size, "The number of references must not change")

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
