package Types.DocumentInfos

import Types.DocReference.DocReference
import Types.{DocumentType, FileType, ReferenceType}

case class CryptolDocumentInfo(
                                override val documentName: String,
                                override val filePath: String,
                                imports: Set[DocReference],
                                types: Set[DocReference],
                                functions: Set[DocReference],
                                properties: Set[DocReference],
                              ) extends DocumentInfo[CryptolDocumentInfo] {

  override val documentType: DocumentType.Value = DocumentType.Cryptol
  val validReferenceTypesTypes: Set[ReferenceType.Value] = Set(ReferenceType.Requirement, ReferenceType.Event, ReferenceType.Import, ReferenceType.Type)
  override val latexLanguageName = "Cryptol"

  require(functions.forall(ref => ref.getReferenceType == ReferenceType.Event && ref.getDocumentType == DocumentType.Cryptol), "All functions must be of type Event")
  require(imports.forall(ref => ref.getReferenceType == ReferenceType.Import && ref.getDocumentType == DocumentType.Cryptol), "All imports must be of type Import")
  require(types.forall(ref => ref.getReferenceType == ReferenceType.Type && ref.getDocumentType == DocumentType.Cryptol), "All types must be of type Type")
  require(properties.forall(ref => ref.getReferenceType == ReferenceType.Requirement && ref.getDocumentType == DocumentType.Cryptol), "All properties must be of type Requirement")

  override lazy val getAllReferences: Set[DocReference] = {
    imports ++ types ++ properties ++ functions
  }

  override def updateReference(ref: DocReference): CryptolDocumentInfo = {
    ref.getReferenceType match {
      case ReferenceType.Import => copy(imports = imports.filterNot(_.getName.equalsIgnoreCase(ref.getName)) + ref)
      case ReferenceType.Type => copy(types = types.filterNot(_.getName.equalsIgnoreCase(ref.getName)) + ref)
      case ReferenceType.Requirement => copy(properties = properties.filterNot(_.getName.equalsIgnoreCase(ref.getName)) + ref)
      case ReferenceType.Event => copy(functions = functions.filterNot(_.getName.equalsIgnoreCase(ref.getName)) + ref)
      case _ => throw new IllegalArgumentException("Invalid reference type")
    }
  }

  override def updateFilePath(newFilePath: String): CryptolDocumentInfo = {
    copy(filePath = newFilePath)
  }

  override def getFileType: FileType.Value = FileType.ComponentFile

  def getImports: Set[DocReference] = imports

  def getTypes: Set[DocReference] = types

  def getFunctions: Set[DocReference] = functions

  def getProperties: Set[DocReference] = properties
}
