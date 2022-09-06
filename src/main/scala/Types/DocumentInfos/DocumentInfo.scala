package Types.DocumentInfos

import Formatter.LatexSanitizer
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
  require(
    {
      val referenceNames = getAllReferences.map(_.getLabelText)
      assert(referenceNames.size == referenceNames.toSet.size, "Reference names must be unique")
      true
    }
  )

  def getLanguage: String = {
    documentType match
      case DocumentType.Lando => "Lando"
      case DocumentType.Lobot => "Lobot"
      case DocumentType.SysML => "SysML"
      case DocumentType.Cryptol => "Cryptol"
      case DocumentType.Saw => "Saw"
      case DocumentType.SV => "System Verilog"
      case DocumentType.BSV => "Bluespec System Verilog"
  }

  def getReferenceName: String = s"${documentType.toString}_${documentName}"

  def getCaption: String = s"${getLanguage} Model of ${LatexSanitizer.sanitizeName(documentName)}."

}












