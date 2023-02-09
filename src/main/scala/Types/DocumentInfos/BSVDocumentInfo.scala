package Types.DocumentInfos

import Types.DocReference.DocReference
import Types.{DocRelation, DocumentType, FileType, ReferenceType}
import Utils.FileUtil

class BSVDocumentInfo(
                       override val documentName: String,
                       override val filePath: String,
                       packages: Set[DocReference],
                       modules: Set[DocReference]
                     ) extends DocumentInfo[BSVDocumentInfo] {

  override val documentType: DocumentType.Value = DocumentType.BSV

  require(FileUtil.getFileType(filePath) == "bsv", "File path must be a BSV file")
  require(packages.intersect(modules).isEmpty, "Packages and modules cannot intersect")
  require(packages.forall(_.getReferenceType == ReferenceType.System))
  require(modules.forall(_.getReferenceType == ReferenceType.SubSystem))

  def copy(
            documentName: String = documentName,
            filePath: String = filePath,
            packages: Set[DocReference] = packages,
            modules: Set[DocReference] = modules,
          ): BSVDocumentInfo = {
    new BSVDocumentInfo(documentName, filePath, packages, modules)
  }

  override def updateReference(ref: DocReference): BSVDocumentInfo = {
    require(ref.getDocumentType == documentType && ref.documentName == documentName, "Can only update references to the same document")
    val newPackages = packages.map(_.updateDocReference(ref))
    val newModules = modules.map(_.updateDocReference(ref))
    copy(packages = newPackages, modules = newModules)
  }

  override def updateFilePath(newFilePath: String): BSVDocumentInfo = {
    require(FileUtil.getFileType(newFilePath) == "bsv", "File path must be a BSV file")
    require(FileUtil.fileExists(newFilePath), "File path must exist")
    copy(filePath = newFilePath)
  }

  private val validReferenceTypesTypes: Set[ReferenceType.Value] = Set(ReferenceType.System, ReferenceType.SubSystem)

  override lazy val getAllReferences: Set[DocReference] = {
    modules ++ packages
  }

  require(getAllReferences.forall(ref => validReferenceTypesTypes.contains(ref.getReferenceType)
    && ref.getDocumentName == documentName
    && ref.getDocumentType == DocumentType.BSV))

  lazy val getRelations: Set[DocRelation] = Set.empty

  override def getFileType: FileType.Value = {
    FileType.ComponentFile
  }
}