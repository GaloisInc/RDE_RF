package Types

import DocumentEnrichers.FileUtil

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
  //      allRefs.filter(_.referenceType == ReferenceType.Import),
  //      allRefs.filter(_.referenceType == ReferenceType.Type),
  //      allRefs.filter(_.referenceType == ReferenceType.Action),
  //      allRefs.filter(_.referenceType == ReferenceType.Requirement)
  //    )


  private val fileUtil = new FileUtil()
  private val validReferenceTypesTypes: Set[ReferenceType] = Set(ReferenceType.Requirement, ReferenceType.Action, ReferenceType.Import, ReferenceType.Type)

  require(getAllReferences.forall(ref => validReferenceTypesTypes.contains(ref.referenceType) && ref.documentType == DocumentType.Cryptol && ref.documentName == documentName))
  require(functions.forall(ref => ref.referenceType == ReferenceType.Action && ref.documentType == DocumentType.Cryptol))
  require(imports.forall(ref => ref.referenceType == ReferenceType.Import && ref.documentType == DocumentType.Cryptol))
  require(types.forall(ref => ref.referenceType == ReferenceType.Type && ref.documentType == DocumentType.Cryptol))
  require(properties.forall(ref => ref.referenceType == ReferenceType.Requirement && ref.documentType == DocumentType.Cryptol))

  override lazy val getAllReferences: Set[DocReference] = {
    imports ++ types ++ properties ++ functions
  }

  lazy val getRelations: Set[DocRelation] = Set.empty

  override def getFileType: FileType = {
    FileType.ComponentFile
  }
}
