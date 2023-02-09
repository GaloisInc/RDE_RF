package Types.DocumentInfos

import Parsers.LobotParser.Analyzer.DocReferencePosition
import Types.DocReference.DocReference
import Types.{DocRelation, DocumentType, FileType, ReferenceType}

class LobotDocumentInfo(
                         override val documentName: String,
                         override val filePath: String,
                         checkDecl: Set[DocReferencePosition],
                         kindDecl: Set[DocReferencePosition],
                         typeDecl: Set[DocReferencePosition],
                         abstTypeDecl: Set[DocReferencePosition],
                         abstFunctionDecl: Set[DocReferencePosition],
                       ) extends DocumentInfo[LobotDocumentInfo] {

  override val documentType: DocumentType.Value = DocumentType.Lobot

  def copy(
            documentName: String = documentName,
            filePath: String = filePath,
            checkDecl: Set[DocReferencePosition] = checkDecl,
            kindDecl: Set[DocReferencePosition] = kindDecl,
            typeDecl: Set[DocReferencePosition] = typeDecl,
            abstTypeDecl: Set[DocReferencePosition] = abstTypeDecl,
            abstFunctionDecl: Set[DocReferencePosition] = abstFunctionDecl,
          ): LobotDocumentInfo = {
    new LobotDocumentInfo(
      documentName,
      filePath,
      checkDecl,
      kindDecl,
      typeDecl,
      abstTypeDecl,
      abstFunctionDecl)
  }

  private val validReferenceTypesTypes: Set[ReferenceType.Value] = Set(ReferenceType.Requirement, ReferenceType.Event, ReferenceType.Import, ReferenceType.Type)

  require(getAllReferences.forall(ref => validReferenceTypesTypes.contains(ref.getReferenceType) && ref.getDocumentType == DocumentType.Lobot && ref.getDocumentName == documentName))
  require(abstTypeDecl.forall(ref => ref.getReferenceType == ReferenceType.Event && ref.getDocumentType == DocumentType.Lobot), "All functions must be of type Event")
  require(typeDecl.forall(ref => ref.getReferenceType == ReferenceType.Type && ref.getDocumentType == DocumentType.Lobot), "All types must be of type Type")
  require(kindDecl.forall(ref => ref.getReferenceType == ReferenceType.Type && ref.getDocumentType == DocumentType.Lobot), "All properties must be of type Requirement")
  require(checkDecl.forall(ref => ref.getReferenceType == ReferenceType.Type && ref.getDocumentType == DocumentType.Lobot), "All properties must be of type Requirement")

  override lazy val getAllReferences: Set[DocReference] = {
    (checkDecl ++ kindDecl ++ typeDecl ++ abstTypeDecl ++ abstFunctionDecl).map(ref => ref.asInstanceOf[DocReference])
  }

  override def updateReference(ref: DocReference): LobotDocumentInfo = {
    ref.getReferenceType match {
      case ReferenceType.Type => copy(typeDecl = typeDecl)
      case _ => throw new IllegalArgumentException("Invalid reference type")
    }
  }

  override def updateFilePath(newFilePath: String): LobotDocumentInfo = {
    copy(filePath = newFilePath)
  }

  lazy val getRelations: Set[DocRelation] = Set.empty

  override def getFileType: FileType.Value = {
    FileType.RequirementFile
  }

}
