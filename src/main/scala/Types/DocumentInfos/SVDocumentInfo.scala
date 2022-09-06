package Types.DocumentInfos

import Types.*
import Utils.FileUtil

class SVDocumentInfo(
                      override val documentName: String,
                      override val filePath: String,
                      modules: Set[DocReference],
                      override val documentType: DocumentType = DocumentType.SV,
                    ) extends DocumentInfo {

  private val validReferenceTypesTypes: Set[ReferenceType] = Set(ReferenceType.System)
  require(getAllReferences.forall(ref => validReferenceTypesTypes.contains(ref.getReferenceType)
    && ref.getDocumentName == documentName
    && ref.getDocumentType == DocumentType.SV))

  override lazy val getAllReferences: Set[DocReference] = {
    modules
  }

  lazy val getRelations: Set[DocRelation] = Set.empty

  override def getFileType: FileType = {
    FileType.ComponentFile
  }
}
