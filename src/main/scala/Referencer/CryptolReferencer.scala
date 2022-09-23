package Referencer

import Types.DocumentInfos.{CryptolDocumentInfo, DocumentInfo, DocumentInfoCompare}
import Types.{DocumentType, ReferenceType}

class CryptolReferencer extends Referencer {
  
  override def addRefinementRelations(documentToExtend: DocumentInfo, abstractDocuments: Array[DocumentInfo], refinedDocuments: Array[DocumentInfo]): DocumentInfo = {
    require(abstractDocuments.forall(_.documentType == DocumentType.SysML), "All abstract documents must be SysML documents")
    require(documentToExtend.documentType == DocumentType.Cryptol, "The document to extend must be a Cryptol document")
    require(refinedDocuments.forall(doc => Set(DocumentType.SV, DocumentType.BSV).contains(doc.documentType)), "All refined documents must be Cryptol documents")
    addAbstractionsToDocument(documentToExtend, abstractDocuments)
  } ensuring ((resDoc: DocumentInfo) => DocumentInfoCompare.compare(resDoc, documentToExtend))

  override def addAbstractionsToDocument(refinedDocument: DocumentInfo, documentsBeingRefined: Array[DocumentInfo]): DocumentInfo = {
    require(documentsBeingRefined.forall(_.documentType == DocumentType.SysML), "All documents being refined must be SysML documents")
    require(refinedDocument.documentType == DocumentType.Cryptol, "The refined document must be a Cryptol document")

    val sysmlReferences = documentsBeingRefined.flatMap(doc => doc.getAllReferences)
    val updatedReferences = refinedDocument.getAllReferences.map(reference => findRefinementRelation(reference, sysmlReferences.toSet))

    new CryptolDocumentInfo(
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
  } ensuring((resDoc: DocumentInfo) => resDoc == documentInfo, "Nothing refines the Cryptol documents.")
}
