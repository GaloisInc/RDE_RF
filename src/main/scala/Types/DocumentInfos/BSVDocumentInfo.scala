package Types.DocumentInfos

import Types.DocReference.DocReference
import Types.{DocRelation, DocumentType, FileType, ReferenceType}
import Utils.FileUtil

class BSVDocumentInfo(
                       override val documentName: String,
                       override val filePath: String,
                       packages: Set[DocReference],
                       modules: Set[DocReference],
                       override val documentType: DocumentType.Value = DocumentType.BSV,
                     ) extends DocumentInfo {

  require(documentName.nonEmpty, "Document name cannot be empty")
  require(filePath.nonEmpty, "File path cannot be empty")
  require(FileUtil.getFileType(filePath) == "bsv", "File path must be a BSV file")
  require(FileUtil.fileExists(filePath), "File path must exist")
  require(packages.intersect(modules).isEmpty, "Packages and modules cannot intersect")

  require(documentType == DocumentType.BSV, "Document type must be BSV")

  require(packages.forall(_.getReferenceType == ReferenceType.System))
  require(modules.forall(_.getReferenceType == ReferenceType.SubSystem))

  def copy(
            documentName: String = documentName,
            filePath: String = filePath,
            packages: Set[DocReference] = packages,
            modules: Set[DocReference] = modules,
            documentType: DocumentType.Value = documentType,
          ): BSVDocumentInfo = {
    new BSVDocumentInfo(documentName, filePath, packages, modules, documentType)
  }

  override def updateReference(ref: DocReference): DocumentInfo = {
    require(ref.getDocumentType == documentType && ref.documentName == documentName, "Can only update references to the same document")
    val newPackages = packages.map(_.updateDocReference(ref))
    val newModules = modules.map(_.updateDocReference(ref))
    copy(packages = newPackages, modules = newModules)
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