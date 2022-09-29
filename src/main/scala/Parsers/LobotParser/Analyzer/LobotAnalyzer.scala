package Parsers.LobotParser.Analyzer

import Parsers.LobotParser.Models._
import Types.DocReference.DocReference
import Types.DocumentInfos.LobotDocumentInfo
import Types.{DocumentType, ReferenceName, ReferenceType}


object LobotAnalyzer {
  private def checkDeclToDocReference(documentName: String, check: CheckDecl): DocReferencePosition = {
    val referenceName = ReferenceName(check.name)
    val lineNumber = check.pos.line
    val originalLine = check.name
    new DocReferencePosition(documentName, referenceName, ReferenceType.Type, DocumentType.Lobot, originalLine, lineNumber)
  }

  private def kindDeclToDocReference(documentName: String, check: KindDecl): DocReferencePosition = {
    val referenceName = ReferenceName(check.name)
    val lineNumber = check.pos.line
    val originalLine = check.name
    new DocReferencePosition(documentName, referenceName, ReferenceType.Type, DocumentType.Lobot, originalLine, lineNumber)
  }

  private def typeDeclToDocReference(documentName: String, check: TypeDecl): DocReferencePosition = {
    val referenceName = ReferenceName(check.name)
    val lineNumber = check.pos.line
    val originalLine = check.name
    val references = check.lobotType match {
      case EnumType(entries) => new DocReferencePosition(documentName, referenceName, ReferenceType.Type, DocumentType.Lobot, originalLine, lineNumber)
      case _ => new DocReferencePosition(documentName, referenceName, ReferenceType.Type, DocumentType.Lobot, originalLine, lineNumber)
    }
    new DocReferencePosition(documentName, referenceName, ReferenceType.Type, DocumentType.Lobot, originalLine, lineNumber)
  }


  def analyzeAST(documentName: String, document: Specification): Unit = {
    require(document.declarations.nonEmpty, "Document is empty")
    require(documentName.nonEmpty, "Document name is empty")

    val references = document.declarations.map {
      case check: CheckDecl => checkDeclToDocReference(documentName, check)
      case kind: KindDecl => kindDeclToDocReference(documentName, kind)
      case typeDecl: TypeDecl => typeDeclToDocReference(documentName, typeDecl)
      case _ => throw new Exception("Unknown declaration")
    }

    new LobotDocumentInfo(
      documentName,
      documentName,
      references.toSet,
      Set.empty,
      Set(),
      Set(),
      Set(),
    )
  }


}


class DocReferencePosition(documentName: String,
                           referenceName: ReferenceName,
                           referenceType: ReferenceType.Value,
                           documentType: DocumentType.Value,
                           override val originalLine: String,
                           lineNumber: Int,
                          )
  extends DocReference(documentName, referenceName, referenceType, documentType, originalLine) {

  require(lineNumber > 0, "Line number must be positive")

  def getLine: Int = lineNumber

}