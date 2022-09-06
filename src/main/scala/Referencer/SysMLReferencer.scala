package Referencer

import Types.DocumentInfos.{CryptolDocumentInfo, DocumentInfo, DocumentInfoCompare, LandoDocumentInfo, SysMLDocumentInfo}
import Types.{DocumentType, ReferenceType}

class SysMLReferencer extends Referencer {
  override def addSpecializationAndAbstract(documentToExtend: DocumentInfo, abstractDocuments: Array[DocumentInfo], specializedDocuments: Array[DocumentInfo]): DocumentInfo = {
    require(abstractDocuments.forall(_.documentType == DocumentType.Lando))
    require(specializedDocuments.forall(_.documentType == DocumentType.Cryptol))
    require(documentToExtend.documentType == DocumentType.SysML)

    val documentWithSpecializations = this.addSpecializationsToDocument(documentToExtend, specializedDocuments)
    this.addAbstractionsToDocument(documentWithSpecializations, abstractDocuments)
  } ensuring ((resDoc: DocumentInfo) => DocumentInfoCompare.compare(resDoc, documentToExtend))


  override def addAbstractionsToDocument(documentInfo: DocumentInfo, landoDocuments: Array[DocumentInfo]): DocumentInfo = {
    require(landoDocuments.forall(_.documentType == DocumentType.Lando))
    require(documentInfo.documentType == DocumentType.SysML)

    val landoReferences = landoDocuments.flatMap(doc => doc.getAllReferences)
    val updatedReferences = documentInfo.getAllReferences.map(sysmlReference => addAbstractions(sysmlReference, landoReferences.toSet))

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

  override def addSpecializationsToDocument(documentInfo: DocumentInfo, cryptolDocuments: Array[DocumentInfo]): DocumentInfo = {
    require(cryptolDocuments.forall(_.documentType == DocumentType.Cryptol))
    require(documentInfo.documentType == DocumentType.SysML)

    val cryptolReferences = cryptolDocuments.flatMap(doc => doc.getAllReferences.filter(ref => ref.getSpecializes.nonEmpty))
    val updatedReferences = documentInfo.getAllReferences.map(sysmlRef => addSpecializations(sysmlRef, cryptolReferences.toSet))

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
