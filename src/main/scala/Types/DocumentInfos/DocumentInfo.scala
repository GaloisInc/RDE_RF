package Types.DocumentInfos

import Formatter.LatexSanitizer
import Types.DocReference.DocReference
import Types.{DocRelation, DocumentType, FileType}
import Utils.FileUtil

abstract class DocumentInfo[+T <: DocumentInfo[T]] {
  def documentName: String

  def filePath: String

  def documentType: DocumentType.Value

  def getFileType: FileType.Value

  def getAllReferences: Set[DocReference]

  def getRelations: Set[DocRelation]

  require(documentName.nonEmpty, "Document name cannot be empty")
  require(filePath.nonEmpty, "File path cannot be empty")
  require(filePath.contains(documentName), "File path must contain document name")
  require(FileUtil.fileExists(filePath), s"File $filePath does not exist")
  require(getAllReferences.forall(_.getDocumentName == documentName), "All references must be to the same document")

  def getLanguage: String = {
    documentType match {
      case DocumentType.Lando => "Lando"
      case DocumentType.Lobot => "Lobot"
      case DocumentType.SysML => "SysML"
      case DocumentType.Cryptol => "Cryptol"
      case DocumentType.Saw => "Saw"
      case DocumentType.SV => "Verilog"
      case DocumentType.BSV => "Verilog"
      case DocumentType.C => "CStyle"
    }
  }

  def getReferenceName: String = s"${documentType.toString}_$documentName"

  def getCaption: String = s"$getLanguage Model of ${LatexSanitizer.sanitizeName(documentName)}"

  def updateReference(ref: DocReference): T

  def updateReferences(refs: Set[DocReference]): T = {
    refs.foldLeft(this.asInstanceOf[T])((doc, ref) => doc.updateReference(ref))
  }

  def updateFilePath(newFilePath: String): T

}










