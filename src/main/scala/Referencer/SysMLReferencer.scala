package Referencer

import Types.DocumentInfos.{CryptolDocumentInfo, DocumentInfo, DocumentInfoCompare, LandoDocumentInfo, SysMLDocumentInfo}
import Types.{DocumentType, ReferenceType}

class SysMLReferencer extends Referencer {
  override def addRefinementRelations(documentToExtend: DocumentInfo, abstractDocuments: Array[DocumentInfo], refinedDocuments: Array[DocumentInfo]): DocumentInfo = {
    require(abstractDocuments.forall(_.documentType == DocumentType.Lando))
    require(refinedDocuments.forall(_.documentType == DocumentType.Cryptol))
    require(documentToExtend.documentType == DocumentType.SysML)

    val documentWithSpecializations = this.addSpecializationsToDocument(documentToExtend, refinedDocuments)
    this.addAbstractionsToDocument(documentWithSpecializations, abstractDocuments)
  } ensuring ((resDoc: DocumentInfo) => DocumentInfoCompare.compare(resDoc, documentToExtend))


  override def addAbstractionsToDocument(documentInfo: DocumentInfo, documentsBeingRefined: Array[DocumentInfo]): DocumentInfo = {
    require(documentsBeingRefined.forall(_.documentType == DocumentType.Lando))
    require(documentInfo.documentType == DocumentType.SysML)

    val landoReferences = documentsBeingRefined.flatMap(doc => doc.getAllReferences)
    val updatedReferences = documentInfo.getAllReferences.map(sysmlReference => findRefinementRelation(sysmlReference, landoReferences.toSet))

    SysMLDocumentInfo(
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
      updatedReferences.filter(_.getReferenceType == ReferenceType.Component)
    )
  } ensuring ((resDoc: SysMLDocumentInfo) => DocumentInfoCompare.compare(resDoc, documentInfo))

  override def addSpecializationsToDocument(documentInfo: DocumentInfo, refinedDocuments: Array[DocumentInfo]): DocumentInfo = {
    require(refinedDocuments.forall(_.documentType == DocumentType.Cryptol))
    require(documentInfo.documentType == DocumentType.SysML)

    val cryptolReferences = refinedDocuments.flatMap(doc => doc.getAllReferences.filter(ref => ref.getAbstractions.nonEmpty))
    val updatedReferences = documentInfo.getAllReferences.map(sysmlRef => addRefinements(sysmlRef, cryptolReferences.toSet))

    SysMLDocumentInfo(
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
      updatedReferences.filter(_.getReferenceType == ReferenceType.Component)
    )
  } ensuring ((resDoc: SysMLDocumentInfo) => DocumentInfoCompare.compare(resDoc, documentInfo))
}
