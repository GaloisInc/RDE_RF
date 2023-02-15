package Types.DocumentInfos

import Types.DocReference.DocReference
import Types.FRET.{FRETRequirement, FRETVariable}
import Types.{DocumentType, FileType, ReferenceType}

class FretDocument(
                    override val documentName: String,
                    override val filePath: String,
                    val requirements: List[FRETRequirement],
                    val variables: List[FRETVariable] = List.empty[FRETVariable]
                  ) extends DocumentInfo[FretDocument] {

  override val documentType: DocumentType.Value = DocumentType.Fret
  val validReferenceTypesTypes: Set[ReferenceType.Value] = Set(ReferenceType.Requirement)
  override val latexLanguageName = "json"

  def copy(
            documentName: String = documentName,
            filePath: String = filePath,
            requirements: List[FRETRequirement] = requirements,
            variables: List[FRETVariable] = variables
          ): FretDocument = {
    new FretDocument(
      documentName,
      filePath,
      requirements,
      variables
    )
  }

  override def updateReference(ref: DocReference): FretDocument = {
    ref.getReferenceType match {
      case ReferenceType.Type => this
      case _ => throw new IllegalArgumentException("Invalid reference type")
    }
  }

  override def updateFilePath(newFilePath: String): FretDocument = {
    copy(filePath = newFilePath)
  }

  override def getFileType: FileType.Value = FileType.RequirementFile
}
