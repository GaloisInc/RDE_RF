package Types.DocumentInfos

import Types.DocReference.DocReference
import Types.{DocRelation, DocumentType, FileType, ReferenceType}
import Utils.FileUtil

class SawDocumentInfo(
                       override val documentName: String,
                       override val filePath: String,
                     ) extends DocumentInfo[SawDocumentInfo] {

  require(FileUtil.getFileType(filePath) == "saw", "File path must be a Saw file")
  override val documentType: DocumentType.Value = DocumentType.Saw

  def copy(
            documentName: String = documentName,
            filePath: String = filePath,
          ): SawDocumentInfo = {
    new SawDocumentInfo(documentName, filePath)
  }

  override def updateReference(ref: DocReference): SawDocumentInfo = {
    copy()
  }

  override def updateFilePath(newFilePath: String): SawDocumentInfo = {
    require(FileUtil.getFileType(newFilePath) == "saw", "File path must be a Saw file")
    require(FileUtil.fileExists(newFilePath), "File path must exist")
    copy(filePath = newFilePath)
  }

  private val validReferenceTypesTypes: Set[ReferenceType.Value] = Set(ReferenceType.System, ReferenceType.SubSystem)

  override lazy val getAllReferences: Set[DocReference] = {
    Set.empty[DocReference]
  }

  require(getAllReferences.forall(ref => validReferenceTypesTypes.contains(ref.getReferenceType)
    && ref.getDocumentName == documentName
    && ref.getDocumentType == DocumentType.Saw))

  lazy val getRelations: Set[DocRelation] = Set.empty[DocRelation]

  override def getFileType: FileType.Value = {
    FileType.ComponentFile
  }
}
