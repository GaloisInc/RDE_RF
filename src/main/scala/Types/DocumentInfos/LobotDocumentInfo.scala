package Types.DocumentInfos

import Parsers.LobotParser.Analyzer.DocReferencePosition
import Types.DocReference.DocReference
import Types.{DocumentType, FileType, ReferenceType}

case class LobotDocumentInfo(
                              override val documentName: String,
                              override val filePath: String,
                              checkDecl: Set[DocReferencePosition],
                              kindDecl: Set[DocReferencePosition],
                              typeDecl: Set[DocReferencePosition],
                              abstTypeDecl: Set[DocReferencePosition],
                              abstFunctionDecl: Set[DocReferencePosition],
                            ) extends DocumentInfo[LobotDocumentInfo] {

  override val documentType: DocumentType.Value = DocumentType.Lobot

  val validReferenceTypesTypes: Set[ReferenceType.Value] = Set(ReferenceType.Requirement, ReferenceType.Event, ReferenceType.Import, ReferenceType.Type)
  override val latexLanguageName = "Lobot"

  require(abstTypeDecl.forall(ref => ref.getReferenceType == ReferenceType.Event), "All functions must be of type Event")
  require(typeDecl.forall(ref => ref.getReferenceType == ReferenceType.Type), "All types must be of type Type")
  require(kindDecl.forall(ref => ref.getReferenceType == ReferenceType.Type), "All properties must be of type Requirement")
  require(checkDecl.forall(ref => ref.getReferenceType == ReferenceType.Type), "All properties must be of type Requirement")

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

  override def getFileType: FileType.Value = {
    FileType.RequirementFile
  }
}