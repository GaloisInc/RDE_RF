package Types.DocumentInfos

import DocumentEnrichers.DocumentEnricher
import Formatter.LatexSanitizer
import Types.DocReference.DocReference
import Types.{DocRelation, DocumentType, FileType, ReferenceType}
import Utils.FileUtil

import java.nio.file.Paths

abstract class DocumentInfo[T <: DocumentInfo[T]] {
  def documentName: String

  def filePath: String

  def documentType: DocumentType.Value

  def getFileType: FileType.Value

  def getAllReferences: Set[DocReference] = Set.empty[DocReference]

  def getRelations: Set[DocRelation] = Set.empty[DocRelation]

  def validReferenceTypesTypes: Set[ReferenceType.Value]

  require(documentName.nonEmpty, "Document name cannot be empty")
  require(filePath.nonEmpty, "File path cannot be empty")
  require(filePath.contains(documentName), "File path must contain document name")
  require(FileUtil.fileExists(filePath), s"File $filePath does not exist")
  require(getAllReferences.forall(_.getDocumentName == documentName), "All references must be to the same document")
  // Todo check if this is still necessary
  /*
  require(getAllReferences.forall(ref1 => getAllReferences.forall(ref2 => ref1.originalLine != ref2.originalLine || ref1 == ref2)),
    "All references must be unique. The following references are not unique: " +
      getAllReferences.groupBy(_.originalLine).filter(_._2.size > 1).values.flatten.map(_.originalLine).mkString(", ") +
    " in file " + filePath)
   */
  def latexLanguageName: String

  def getReferenceName: String = s"${documentType.toString}_$documentName"

  def getCaption: String = s"model of ${LatexSanitizer.sanitizeName(documentName)}"

  def updateReference(ref: DocReference): T

  def updateReferences(refs: Set[DocReference]): T = {
    val document = refs.foldLeft(this.asInstanceOf[T])((doc, ref) => doc.updateReference(ref))
    document
  } ensuring((doc: T) =>
    doc.documentName == documentName &&
      doc.filePath == filePath &&
      doc.getAllReferences.size == getAllReferences.size,
    "Document must not change for updateReferences " + documentName + " " + filePath +
      " had original " + getAllReferences.size + " references.")

  def moveFile(destination: String): T = {
    val oldFile = Paths.get(filePath)
    val destinationPath = Paths.get(destination, documentType.toString).toString
    val newFilePath = FileUtil.moveRenameFile(filePath, destinationPath)
    val updatedDocument = updateFilePath(newFilePath)
    oldFile.toFile.delete()
    updatedDocument
  }

  def updateFilePath(newFilePath: String): T

  def decorate[D <: DocumentEnricher[T]](decorator: D): T = {
    val newFilePath = decorator.decorateFile(this.asInstanceOf[T])
    updateFilePath(newFilePath)
  }
}










