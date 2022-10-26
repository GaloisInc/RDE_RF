package Types.DocumentInfos

import Types.DocReference.DocReference
import Types.{DocRelation, DocumentType, FileType, ReferenceType}
import Utils.FileUtil

class CDocumentInfo(
                       override val documentName: String,
                       override val filePath: String,
                       override val documentType: DocumentType.Value = DocumentType.C,
                     ) extends DocumentInfo {

  require(documentName.nonEmpty, "Document name cannot be empty")
  require(filePath.nonEmpty, "File path cannot be empty")
  require(FileUtil.getFileType(filePath) == "c", "File path must be a C file")
  require(FileUtil.fileExists(filePath), "File path must exist")
  require(documentType == DocumentType.C, "Document type must be C")

  def copy(
            documentName: String = documentName,
            filePath: String = filePath,
            documentType: DocumentType.Value = documentType,
          ): CDocumentInfo = {
    new CDocumentInfo(documentName, filePath, documentType)
  }

  override def updateReference(ref: DocReference): DocumentInfo = {
    copy()
  }


  override lazy val getAllReferences: Set[DocReference] = {
    Set.empty[DocReference]
  }

  lazy val getRelations: Set[DocRelation] = Set.empty[DocRelation]

  override def getFileType: FileType.Value = {
    FileType.ComponentFile
  }
}
