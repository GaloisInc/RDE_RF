package Referencer

import Types.DocumentInfos.{CryptolDocumentInfo, DocumentInfo, DocumentInfoCompare, LandoDocumentInfo, SysMLDocumentInfo}
import Types.{DocumentType, ReferenceType}

class LandoReferencer extends Referencer {
  def addSpecializationAndAbstract(documentToExtend: DocumentInfo, abstractDocuments: Array[DocumentInfo], specializedDocuments: Array[DocumentInfo]): DocumentInfo = {
    require(specializedDocuments.forall(_.documentType == DocumentType.SysML), "All specialized documents must be SysML documents")
    require(documentToExtend.documentType == DocumentType.Lando, "The document to extend must be a Lando document")
    addSpecializationsToDocument(documentToExtend, specializedDocuments)
  } ensuring ((resDoc: DocumentInfo) => DocumentInfoCompare.compare(resDoc, documentToExtend))

  def addAbstractionsToDocument(specializedDocument: DocumentInfo, abstractDocument: Array[DocumentInfo]): DocumentInfo = {
    specializedDocument
  } ensuring ((res: DocumentInfo) => res == specializedDocument, "The result is the same as the input - no abstractions are defined for Lando.")

  def addSpecializationsToDocument(documentInfo: DocumentInfo, sysMLDocuments: Array[DocumentInfo]): DocumentInfo = {
    require(sysMLDocuments.forall(_.documentType == DocumentType.SysML), "All specialization documents for Lando must be SysML documents.")
    require(documentInfo.documentType == DocumentType.Lando, "The document to extend must be a Lando document.")

    val sysmlReferences = sysMLDocuments.flatMap(doc => doc.getAllReferences.filter(ref => ref.getSpecializes.nonEmpty))
    val updatedReferences = documentInfo.getAllReferences.map(landoRef => addSpecializations(landoRef, sysmlReferences.toSet))

    val componentSet = Set(ReferenceType.Component, ReferenceType.SubSystem, ReferenceType.System)
    LandoDocumentInfo(
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






