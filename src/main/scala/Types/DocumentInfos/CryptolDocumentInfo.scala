package Types.DocumentInfos

import Types.DocReference.DocReference
import Types.{DocRelation, DocumentType, FileType, ReferenceType}

class CryptolDocumentInfo(
                           override val documentName: String,
                           override val filePath: String,
                           imports: Set[DocReference],
                           types: Set[DocReference],
                           functions: Set[DocReference],
                           properties: Set[DocReference],
                         ) extends DocumentInfo[CryptolDocumentInfo] {

  override val documentType: DocumentType.Value = DocumentType.Cryptol

  def copy(
            documentName: String = documentName,
            filePath: String = filePath,
            imports: Set[DocReference] = imports,
            types: Set[DocReference] = types,
            functions: Set[DocReference] = functions,
            properties: Set[DocReference] = properties): CryptolDocumentInfo = {
    new CryptolDocumentInfo(
      documentName,
      filePath,
      imports,
      types,
      functions,
      properties
    )
  }

  private val validReferenceTypesTypes: Set[ReferenceType.Value] = Set(ReferenceType.Requirement, ReferenceType.Event, ReferenceType.Import, ReferenceType.Type)

  require(getAllReferences.forall(ref => validReferenceTypesTypes.contains(ref.getReferenceType) && ref.getDocumentType == DocumentType.Cryptol && ref.getDocumentName == documentName))
  require(functions.forall(ref => ref.getReferenceType == ReferenceType.Event && ref.getDocumentType == DocumentType.Cryptol), "All functions must be of type Event")
  require(imports.forall(ref => ref.getReferenceType == ReferenceType.Import && ref.getDocumentType == DocumentType.Cryptol), "All imports must be of type Import")
  require(types.forall(ref => ref.getReferenceType == ReferenceType.Type && ref.getDocumentType == DocumentType.Cryptol), "All types must be of type Type")
  require(properties.forall(ref => ref.getReferenceType == ReferenceType.Requirement && ref.getDocumentType == DocumentType.Cryptol), "All properties must be of type Requirement")

  override lazy val getAllReferences: Set[DocReference] = {
    imports ++ types ++ properties ++ functions
  }

  override def updateReference(ref: DocReference): CryptolDocumentInfo = {
    ref.getReferenceType match {
      case ReferenceType.Import => copy(imports = imports.map(_.updateDocReference(ref)))
      case ReferenceType.Type => copy(types = types.map(_.updateDocReference(ref)))
      case ReferenceType.Requirement => copy(properties = properties.map(_.updateDocReference(ref)))
      case ReferenceType.Event => copy(functions = functions.map(_.updateDocReference(ref)))
      case _ => throw new IllegalArgumentException("Invalid reference type")
    }
  }

  override def updateFilePath(newFilePath: String): CryptolDocumentInfo = {
    copy(filePath = newFilePath)
  }

  lazy val getRelations: Set[DocRelation] = Set.empty

  override def getFileType: FileType.Value = {
    FileType.ComponentFile
  }

  def getImports: Set[DocReference] = imports

  def getTypes: Set[DocReference] = types

  def getFunctions: Set[DocReference] = functions

  def getProperties: Set[DocReference] = properties
}
