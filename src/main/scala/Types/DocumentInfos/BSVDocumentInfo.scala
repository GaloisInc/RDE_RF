package Types.DocumentInfos

import Types.DocReference.DocReference
import Types.{DocumentType, FileType, ReferenceType}
import Utils.FileUtil

case class BSVDocumentInfo(
                            override val documentName: String,
                            override val filePath: String,
                            packages: Set[DocReference],
                            modules: Set[DocReference]
                          ) extends DocumentInfo[BSVDocumentInfo] {

  override val documentType: DocumentType.Value = DocumentType.BSV
  val validReferenceTypesTypes: Set[ReferenceType.Value] = Set(ReferenceType.System, ReferenceType.SubSystem)
  override val latexLanguageName = "Verilog"

  require(FileUtil.getFileType(filePath) == "bsv", "File path must be a BSV file")
  require(packages.intersect(modules).isEmpty, "Packages and modules cannot intersect")
  require(packages.forall(_.getReferenceType == ReferenceType.System))
  require(modules.forall(_.getReferenceType == ReferenceType.SubSystem))

  override def updateReference(ref: DocReference): BSVDocumentInfo = {
    require(ref.getDocumentType == documentType && ref.documentName == documentName, "Can only update references to the same document")
    ref.getReferenceType match {
      case ReferenceType.System => copy(packages = packages.filterNot(_.getOriginalLine.equalsIgnoreCase(ref.getOriginalLine)) + ref)
      case ReferenceType.SubSystem => copy(modules = modules.filterNot(_.getOriginalLine.equalsIgnoreCase(ref.getOriginalLine)) + ref)
      case _ => throw new IllegalArgumentException("Invalid reference type")
    }
  } ensuring((newDoc: BSVDocumentInfo) => newDoc.getAllReferences.size == getAllReferences.size, "Number of references should not change")

  override def updateFilePath(newFilePath: String): BSVDocumentInfo = {
    require(FileUtil.getFileType(newFilePath) == "bsv", "File path must be a BSV file")
    require(FileUtil.fileExists(newFilePath), "File path must exist")
    copy(filePath = newFilePath)
  }


  override lazy val getAllReferences: Set[DocReference] = {
    modules ++ packages
  }

  override def getFileType: FileType.Value = FileType.ComponentFile
}