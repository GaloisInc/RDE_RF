package Types.DocumentInfos

import Types.DocReference.DocReference
import Types.{DocRelation, DocumentType, FileType, ReferenceType}

class SVDocumentInfo(
                      override val documentName: String,
                      override val filePath: String,
                      modules: Set[DocReference],
                      override val documentType: DocumentType.Value = DocumentType.SV,
                    ) extends DocumentInfo {

  def copy(
            documentName: String = documentName,
            filePath: String = filePath,
            modules: Set[DocReference] = modules,
            documentType: DocumentType.Value = documentType,
          ): SVDocumentInfo = {
    new SVDocumentInfo(documentName, filePath, modules, documentType)
  }

  override def updateReference(ref: DocReference): DocumentInfo = {
    require(ref.getReferenceType == ReferenceType.System, "SVDocumentInfo can only update module references")
    ref.getReferenceType match {
      case ReferenceType.System => copy(modules = modules.map(_.updateDocReference(ref)))
      case _ => throw new IllegalArgumentException("Invalid reference type")
    }
  }

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
