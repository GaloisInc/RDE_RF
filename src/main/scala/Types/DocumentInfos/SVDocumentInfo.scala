package Types.DocumentInfos

import Types.DocReference.DocReference
import Types.{DocRelation, DocumentType, FileType, ReferenceType}

class SVDocumentInfo(
                      override val documentName: String,
                      override val filePath: String,
                      modules: Set[DocReference],
                      override val documentType: DocumentType.Value = DocumentType.SV,
                    ) extends DocumentInfo {

  private val validReferenceTypesTypes: Set[ReferenceType.Value] = Set(ReferenceType.System)
  require(getAllReferences.forall(ref => validReferenceTypesTypes.contains(ref.getReferenceType)
    && ref.getDocumentName == documentName
    && ref.getDocumentType == DocumentType.SV))

  override lazy val getAllReferences: Set[DocReference] = {
    modules
  }

  lazy val getRelations: Set[DocRelation] = Set.empty

  override def getFileType: FileType.Value = {
    FileType.ComponentFile
  }
}
