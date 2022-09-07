package Referencer

import Types.*
import Types.DocumentInfos.{CryptolDocumentInfo, DocumentInfo, DocumentInfoCompare}

class CryptolReferencer extends Referencer {
  override def addRefinementRelations(documentToExtend: DocumentInfo, abstractDocuments: Array[DocumentInfo], refinedDocuments: Array[DocumentInfo]): DocumentInfo = {
    require(abstractDocuments.forall(_.documentType == DocumentType.SysML))
    require(documentToExtend.documentType == DocumentType.Cryptol)
    addAbstractionsToDocument(documentToExtend, abstractDocuments)
  } ensuring ((resDoc: DocumentInfo) => DocumentInfoCompare.compare(resDoc, documentToExtend))

  override def addAbstractionsToDocument(refinedDocument: DocumentInfo, documentsBeingRefined: Array[DocumentInfo]): DocumentInfo = {
    require(documentsBeingRefined.forall(_.documentType == DocumentType.SysML))
    require(refinedDocument.documentType == DocumentType.Cryptol)

    val sysmlReferences = documentsBeingRefined.flatMap(doc => doc.getAllReferences)
    val updatedReferences = refinedDocument.getAllReferences.map(reference => findRefinementRelation(reference, sysmlReferences.toSet))

    CryptolDocumentInfo(
      refinedDocument.documentName,
      refinedDocument.filePath,
      updatedReferences.filter(_.getReferenceType == ReferenceType.Import),
      updatedReferences.filter(_.getReferenceType == ReferenceType.Type),
      updatedReferences.filter(_.getReferenceType == ReferenceType.Event),
      updatedReferences.filter(_.getReferenceType == ReferenceType.Requirement),
    )
  } ensuring ((resDoc: DocumentInfo) => DocumentInfoCompare.compare(resDoc, refinedDocument))

  override def addSpecializationsToDocument(documentInfo: DocumentInfo, refinedDocuments: Array[DocumentInfo]): DocumentInfo = {
    documentInfo
  } ensuring ((resDoc: DocumentInfo) => resDoc == documentInfo, "Nothing refines the Cryptol documents.")
}
