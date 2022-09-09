package Types.DocumentInfos

import Types.*
import Types.DocReference.DocReference
import Utils.FileUtil

class BSVDocumentInfo(
                      override val documentName: String,
                      override val filePath: String,
                      packages: Set[DocReference],
                      modules: Set[DocReference],
                      override val documentType: DocumentType = DocumentType.BSV,
                    ) extends DocumentInfo {

  def copy(
            documentName: String = documentName,
            filePath: String = filePath,
            packages: Set[DocReference] = packages,
            modules: Set[DocReference] = modules,
            documentType: DocumentType = documentType,
          ): BSVDocumentInfo = {
    new BSVDocumentInfo(documentName, filePath, packages, modules, documentType)
  }

  private val validReferenceTypesTypes: Set[ReferenceType] = Set(ReferenceType.System, ReferenceType.SubSystem)
  require(getAllReferences.forall(ref => validReferenceTypesTypes.contains(ref.getReferenceType)
    && ref.getDocumentName == documentName
    && ref.getDocumentType == DocumentType.BSV))

  require(packages.forall(_.getReferenceType == ReferenceType.System))
  require(modules.forall(_.getReferenceType == ReferenceType.SubSystem))

  override lazy val getAllReferences: Set[DocReference] = {
    modules ++ packages
  }

  lazy val getRelations: Set[DocRelation] = Set.empty

  override def getFileType: FileType = {
    FileType.ComponentFile
  }
}
