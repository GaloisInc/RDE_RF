package Referencer

import Types.DocumentInfos.{DocumentInfo, DocumentInfoCompare, SysMLDocumentInfo}
import Types.{DocumentType, ReferenceType}

class SysMLReferencer extends Referencer {
  override def addRefinementRelations(documentToExtend: DocumentInfo, abstractDocuments: Array[DocumentInfo], refinedDocuments: Array[DocumentInfo]): DocumentInfo = {
    require(abstractDocuments.forall(_.documentType == DocumentType.Lando), "All abstract documents must be Lando documents")
    require(refinedDocuments.forall(_.documentType == DocumentType.Cryptol), "All refined documents must be Cryptol documents")
    require(documentToExtend.documentType == DocumentType.SysML, "The document to extend must be a SysML document")

    logger.info(s"Adding refinement relations to ${documentToExtend.documentName}")
    val documentWithSpecializations = this.addSpecializationsToDocument(documentToExtend, refinedDocuments)
    this.addAbstractionsToDocument(documentWithSpecializations, abstractDocuments)
  } ensuring ((resDoc: DocumentInfo) => DocumentInfoCompare.compare(resDoc, documentToExtend))

  override def addAbstractionsToDocument(documentInfo: DocumentInfo, documentsBeingRefined: Array[DocumentInfo]): DocumentInfo = {
    require(documentsBeingRefined.forall(_.documentType == DocumentType.Lando), "All documents being refined must be Lando documents")
    require(documentInfo.documentType == DocumentType.SysML, "The document to extend must be a SysML document")
    logger.info(s"Adding abstractions to ${documentInfo.documentName}")

    val landoReferences = documentsBeingRefined.flatMap(doc => doc.getAllReferences)
    val updatedReferences = documentInfo.getAllReferences.map(sysmlReference => findRefinementRelation(sysmlReference, landoReferences.toSet))

    new SysMLDocumentInfo(
      documentInfo.documentName,
      documentInfo.filePath,
      updatedReferences.filter(_.getReferenceType == ReferenceType.System),
      updatedReferences.filter(_.getReferenceType == ReferenceType.SubSystem),
      updatedReferences.filter(_.getReferenceType == ReferenceType.Connection),
      updatedReferences.filter(_.getReferenceType == ReferenceType.Scenario),
      updatedReferences.filter(_.getReferenceType == ReferenceType.Requirement),
      updatedReferences.filter(_.getReferenceType == ReferenceType.Event),
      updatedReferences.filter(_.getReferenceType == ReferenceType.Import),
      updatedReferences.filter(_.getReferenceType == ReferenceType.View),
      updatedReferences.filter(_.getReferenceType == ReferenceType.Component),
      updatedReferences.filter(_.getReferenceType == ReferenceType.Attribute),
    )
  } ensuring ((resDoc: SysMLDocumentInfo) => DocumentInfoCompare.compare(resDoc, documentInfo))

  override def addSpecializationsToDocument(documentInfo: DocumentInfo, refinedDocuments: Array[DocumentInfo]): DocumentInfo = {
    require(refinedDocuments.forall(_.documentType == DocumentType.Cryptol), "All refined documents must be Cryptol documents")
    require(documentInfo.documentType == DocumentType.SysML, "The document to extend must be a SysML document")

    logger.info(s"Adding specializations to ${documentInfo.documentName}")
    val cryptolReferences = refinedDocuments.flatMap(doc => doc.getAllReferences.filter(ref => ref.getAbstractions.nonEmpty))
    val updatedReferences = documentInfo.getAllReferences.map(sysmlRef => addRefinements(sysmlRef, cryptolReferences.toSet))

    new SysMLDocumentInfo(
      documentInfo.documentName,
      documentInfo.filePath,
      updatedReferences.filter(_.getReferenceType == ReferenceType.System),
      updatedReferences.filter(_.getReferenceType == ReferenceType.SubSystem),
      updatedReferences.filter(_.getReferenceType == ReferenceType.Connection),
      updatedReferences.filter(_.getReferenceType == ReferenceType.Scenario),
      updatedReferences.filter(_.getReferenceType == ReferenceType.Requirement),
      updatedReferences.filter(_.getReferenceType == ReferenceType.Event),
      updatedReferences.filter(_.getReferenceType == ReferenceType.Import),
      updatedReferences.filter(_.getReferenceType == ReferenceType.View),
      updatedReferences.filter(_.getReferenceType == ReferenceType.Component),
      updatedReferences.filter(_.getReferenceType == ReferenceType.Attribute),
    )
  } ensuring ((resDoc: SysMLDocumentInfo) => DocumentInfoCompare.compare(resDoc, documentInfo))
}
