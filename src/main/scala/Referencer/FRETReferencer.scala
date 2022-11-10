package Referencer

import Types.DocumentInfos.{DocumentInfo, DocumentInfoCompare, FRETDocumentInfo}
import Types.{DocumentType, ReferenceType}

class FRETReferencer extends Referencer {
  def addRefinementRelations(documentToExtend: DocumentInfo, abstractDocuments: Array[DocumentInfo], refinedDocuments: Array[DocumentInfo]): DocumentInfo = {
    require(refinedDocuments.forall(_.documentType == DocumentType.SysML), "All specialized documents must be SysML documents")
    require(documentToExtend.documentType == DocumentType.FRET, "The document to extend must be a FRET document")
    addSpecializationsToDocument(documentToExtend, refinedDocuments)
  } ensuring ((resDoc: DocumentInfo) => DocumentInfoCompare.compare(resDoc, documentToExtend))

  def addAbstractionsToDocument(specializedDocument: DocumentInfo, documentsBeingRefined: Array[DocumentInfo]): DocumentInfo = {
    specializedDocument
  } ensuring ((res: DocumentInfo) => res == specializedDocument, "The result is the same as the input - no abstractions are defined for Lando.")

  def addSpecializationsToDocument(documentInfo: DocumentInfo, refinedDocuments: Array[DocumentInfo]): DocumentInfo = {
    require(refinedDocuments.forall(_.documentType == DocumentType.SysML), "All specialization documents for Lando must be SysML documents.")
    require(documentInfo.documentType == DocumentType.FRET, "The document to extend must be a FRET document.")

    val sysmlReferences = refinedDocuments.flatMap(doc => doc.getAllReferences.filter(ref => ref.getAbstractions.nonEmpty))
    val updatedReferences = documentInfo.getAllReferences.map(landoRef => addRefinements(landoRef, sysmlReferences.toSet))

    new FRETDocumentInfo(
      documentInfo.documentName,
      documentInfo.filePath,
      updatedReferences.filter(_.getReferenceType == ReferenceType.Requirement)
    )
  } ensuring ((resDoc: FRETDocumentInfo) => DocumentInfoCompare.compare(resDoc, documentInfo))
}
