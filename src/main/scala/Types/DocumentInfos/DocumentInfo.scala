package Types.DocumentInfos

import Types.{DocReference, DocRelation, DocumentType, FileType}
import Utils.FileUtil

abstract class DocumentInfo {
  def documentName: String

  def filePath: String

  //  def references: String
  def documentType: DocumentType

  def getFileType: FileType

  def getAllReferences: Set[DocReference]

  def getRelations: Set[DocRelation]

  require(documentName.nonEmpty, "Document name cannot be empty")
  require(filePath.contains(documentName), "File path must contain document name")
  //All reference names must be unique
  require(getAllReferences.map(_.getLabelText).size == getAllReferences.size, s"Non unique references in $documentName")


}












