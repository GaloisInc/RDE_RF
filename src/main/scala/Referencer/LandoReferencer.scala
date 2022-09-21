package Referencer

import Types.DocumentInfos.{DocumentInfo, DocumentInfoCompare, LandoDocumentInfo}
import Types.{DocumentType, ReferenceType}

class LandoReferencer extends Referencer {
  def addRefinementRelations(documentToExtend: DocumentInfo, abstractDocuments: Array[DocumentInfo], refinedDocuments: Array[DocumentInfo]): DocumentInfo = {
    require(refinedDocuments.forall(_.documentType == DocumentType.SysML), "All specialized documents must be SysML documents")
    require(documentToExtend.documentType == DocumentType.Lando, "The document to extend must be a Lando document")
    addSpecializationsToDocument(documentToExtend, refinedDocuments)
  } ensuring ((resDoc: DocumentInfo) => DocumentInfoCompare.compare(resDoc, documentToExtend))

  def addAbstractionsToDocument(specializedDocument: DocumentInfo, documentsBeingRefined: Array[DocumentInfo]): DocumentInfo = {
    specializedDocument
  } ensuring ((res: DocumentInfo) => res == specializedDocument, "The result is the same as the input - no abstractions are defined for Lando.")

  def addSpecializationsToDocument(documentInfo: DocumentInfo, refinedDocuments: Array[DocumentInfo]): DocumentInfo = {
    require(refinedDocuments.forall(_.documentType == DocumentType.SysML), "All specialization documents for Lando must be SysML documents.")
    require(documentInfo.documentType == DocumentType.Lando, "The document to extend must be a Lando document.")

    val sysmlReferences = refinedDocuments.flatMap(doc => doc.getAllReferences.filter(ref => ref.getAbstractions.nonEmpty))
    val updatedReferences = documentInfo.getAllReferences.map(landoRef => addRefinements(landoRef, sysmlReferences.toSet))

    val componentSet = Set(ReferenceType.Component, ReferenceType.SubSystem, ReferenceType.System)
    new LandoDocumentInfo(
      documentInfo.documentName,
      documentInfo.filePath,
      updatedReferences.filter(ref => componentSet.contains(ref.getReferenceType)),
      documentInfo.getRelations,
      updatedReferences.filter(_.getReferenceType == ReferenceType.Event),
      updatedReferences.filter(_.getReferenceType == ReferenceType.Requirement),
      updatedReferences.filter(_.getReferenceType == ReferenceType.Scenario),
    )
  } ensuring ((resDoc: LandoDocumentInfo) => DocumentInfoCompare.compare(resDoc, documentInfo))
}






