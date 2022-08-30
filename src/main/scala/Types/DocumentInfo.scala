package Types

import DocumentEnrichers.FileUtil

abstract class DocumentInfo {
  def documentName: String

  def filePath: String

  //  def references: String
  def documentType: DocumentType

  def getFileType: FileType

  def getAllReferences: Set[DocReference]

  def getRelations: Set[DocRelation]

  require(documentName.nonEmpty)
  require(filePath.contains(documentName))
  //All reference names must be unique
  require(getAllReferences.map(ref => ref.referenceName.reference).sizeIs == getAllReferences.sizeIs, s"Non unique references in $documentName")

}












