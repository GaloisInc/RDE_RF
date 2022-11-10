package Types.DocumentInfos

import Types.DocReference.DocReference
import Types.{DocRelation, DocumentType, FileType, ReferenceType}

class FRETDocumentInfo(
                         override val documentName: String,
                         override val filePath: String,
                         requirements: Set[DocReference],
                         override val documentType: DocumentType.Value = DocumentType.FRET,
                       ) extends DocumentInfo {

  require(documentType == DocumentType.FRET, "Document type must be FRET")

  def copy(
            documentName: String = documentName,
            filePath: String = filePath,
            requirements: Set[DocReference] = requirements,
          ): FRETDocumentInfo = {
    new FRETDocumentInfo(
      documentName,
      filePath,
      requirements,
      this.documentType
    )
  }

  override def updateReference(ref: DocReference): DocumentInfo = {
    ref.getReferenceType match {
      case Types.ReferenceType.Requirement =>
        val newRequirements = requirements.filterNot(_.getName.equalsIgnoreCase(ref.getName)) + ref
        copy(requirements = newRequirements)
      case _ => throw new Exception("Unknown reference type")
    }
  }

  // A FRET document can only have requirements as references
  private val validReferenceTypesTypes: Set[ReferenceType.Value] = Set(ReferenceType.Requirement)

  require(getAllReferences.forall(ref => validReferenceTypesTypes.contains(ref.getReferenceType) && ref.getDocumentType == DocumentType.FRET && ref.getDocumentName == documentName))

  override lazy val getAllReferences: Set[DocReference] = requirements

  lazy val getRelations: Set[DocRelation] = Set.empty[DocRelation]

  override def getFileType: FileType.Value = {
    FileType.RequirementFile
  }
}
