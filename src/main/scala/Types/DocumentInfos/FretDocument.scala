package Types.DocumentInfos

import Types.DocReference.DocReference
import Types.FRET.{FRETRequirement, FRETVariable}
import Types.{DocumentType, FileType, ReferenceType}

case class FretDocument(
                         override val documentName: String,
                         override val filePath: String,
                         requirements: List[FRETRequirement],
                         variables: List[FRETVariable] = List.empty[FRETVariable],
                         docRequirements: List[DocReference] = List.empty[DocReference],
                         docVariables: List[DocReference] = List.empty[DocReference]
                       ) extends DocumentInfo[FretDocument] {

  override val documentType: DocumentType.Value = DocumentType.Fret
  val validReferenceTypesTypes: Set[ReferenceType.Value] = Set(ReferenceType.Requirement, ReferenceType.Type)
  override val latexLanguageName = "json"

  override def getAllReferences: Set[DocReference] = docRequirements.toSet ++ docVariables.toSet

  override def updateReference(ref: DocReference): FretDocument = {
    ref.getReferenceType match {
      case ReferenceType.Type => copy(docVariables = docVariables.filterNot(_.originalLine == ref.originalLine) :+ ref)
      case ReferenceType.Requirement => copy(docRequirements = docRequirements.filterNot(_.originalLine == ref.originalLine) :+ ref)
      case _ => throw new IllegalArgumentException("Invalid reference type")
    }
  } ensuring ((doc: FretDocument) => doc.getAllReferences.size == getAllReferences.size)

  override def updateFilePath(newFilePath: String): FretDocument = {
    copy(filePath = newFilePath)
  }

  override def getFileType: FileType.Value = FileType.RequirementFile
}
