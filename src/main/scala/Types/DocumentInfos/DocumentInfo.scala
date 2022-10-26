package Types.DocumentInfos

import Formatter.LatexSanitizer
import Specs.FileSpecs
import Types.DocReference.DocReference
import Types.{DocRelation, DocumentType, FileType}
import Utils.FileUtil

abstract class DocumentInfo {
  def documentName: String

  def filePath: String

  //  def references: String
  def documentType: DocumentType.Value

  def getFileType: FileType.Value

  def getAllReferences: Set[DocReference]

  def getRelations: Set[DocRelation]

  require(documentName.nonEmpty, "Document name cannot be empty")
  require(filePath.contains(documentName), "File path must contain document name")
  require(FileUtil.fileExists(filePath), s"File $filePath does not exist")
  //All reference names must be unique
  require(
    {
      val referenceNames = getAllReferences.map(_.getLabelText)
      assert(referenceNames.size == referenceNames.size, "Reference names must be unique")
      true
    }
  )

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

  def updateReference (ref: DocReference): DocumentInfo

}












