package Types.DocumentInfos

import Types.*
import Utils.FileUtil

class CryptolDocumentInfo(
                           override val documentName: String,
                           override val filePath: String,
                           imports: Set[DocReference],
                           types: Set[DocReference],
                           functions: Set[DocReference],
                           properties: Set[DocReference],
                           override val documentType: DocumentType = DocumentType.Cryptol,
                         ) extends DocumentInfo {


  //  def this(document: String, path: String, allRefs: Set[DocReference]) =
  //    this(document,
  //      path,
  //      allRefs.filter(_.getReferenceType == ReferenceType.Import),
  //      allRefs.filter(_.getReferenceType == ReferenceType.Type),
  //      allRefs.filter(_.getReferenceType == ReferenceType.Event),
  //      allRefs.filter(_.getReferenceType == ReferenceType.Requirement)
  //    )


  private val validReferenceTypesTypes: Set[ReferenceType] = Set(ReferenceType.Requirement, ReferenceType.Event, ReferenceType.Import, ReferenceType.Type)

  require(getAllReferences.forall(ref => validReferenceTypesTypes.contains(ref.getReferenceType) && ref.getDocumentType == DocumentType.Cryptol && ref.getDocumentName == documentName))
  require(functions.forall(ref => ref.getReferenceType == ReferenceType.Event && ref.getDocumentType == DocumentType.Cryptol), "All functions must be of type Event")
  require(imports.forall(ref => ref.getReferenceType == ReferenceType.Import && ref.getDocumentType == DocumentType.Cryptol), "All imports must be of type Import")
  require(types.forall(ref => ref.getReferenceType == ReferenceType.Type && ref.getDocumentType == DocumentType.Cryptol), "All types must be of type Type")
  require(properties.forall(ref => ref.getReferenceType == ReferenceType.Requirement && ref.getDocumentType == DocumentType.Cryptol), "All properties must be of type Requirement")

  override lazy val getAllReferences: Set[DocReference] = {
    imports ++ types ++ properties ++ functions
  }

  lazy val getRelations: Set[DocRelation] = Set.empty

  override def getFileType: FileType = {
    FileType.ComponentFile
  }
}
