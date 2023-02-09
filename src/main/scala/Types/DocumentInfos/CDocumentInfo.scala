package Types.DocumentInfos

import Types.DocReference.DocReference
import Types.{DocRelation, DocumentType, FileType, ReferenceType}
import Utils.FileUtil

class CDocumentInfo(
                     override val documentName: String,
                     override val filePath: String,
                   ) extends DocumentInfo[CDocumentInfo] {

  override val documentType: DocumentType.Value = DocumentType.C

  require(FileUtil.getFileType(filePath) == "c", "File path must be a C file")
  require(FileUtil.fileExists(filePath), "File path must exist")

  def copy(
            documentName: String = documentName,
            filePath: String = filePath,
          ): CDocumentInfo = {
    new CDocumentInfo(documentName, filePath)
  }

  override def updateReference(ref: DocReference): CDocumentInfo = {
    copy()
  }

  override def updateFilePath(newFilePath: String): CDocumentInfo = {
    require(FileUtil.getFileType(newFilePath) == "c", "File path must be a C file")
    require(FileUtil.fileExists(newFilePath), "File path must exist")
    copy(filePath = newFilePath)
  }

  override lazy val getAllReferences: Set[DocReference] = {
    Set.empty[DocReference]
  }

  lazy val getRelations: Set[DocRelation] = Set.empty[DocRelation]

  override def getFileType: FileType.Value = {
    FileType.ComponentFile
  }
}
