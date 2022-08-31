package Referencer

import Types.DocumentInfos.{CryptolDocumentInfo, DocumentInfo, LandoDocumentInfo, SysMLDocumentInfo}
import Types.{DocumentType, ReferenceType}

class SysMLReferencer extends Referencer {
  override def addSpecializationAndAbstract(documentToExtend: DocumentInfo, abstractDocuments: Array[DocumentInfo], specializedDocuments: Array[DocumentInfo]): DocumentInfo = {
    require(abstractDocuments.forall(_.documentType == DocumentType.Lando))
    require(specializedDocuments.forall(_.documentType == DocumentType.Cryptol))
    require(documentToExtend.documentType == DocumentType.SysML)

    val documentWithSpecializations = this.addSpecializationsToDocument(documentToExtend, specializedDocuments)
    this.addAbstractionsToDocument(documentWithSpecializations, abstractDocuments)
  }

  override def addAbstractionsToDocument(documentInfo: DocumentInfo, landoDocuments: Array[DocumentInfo]): DocumentInfo = {
    require(landoDocuments.forall(_.documentType == DocumentType.Lando))
    require(documentInfo.documentType == DocumentType.SysML)

    val landoReferences = landoDocuments.flatMap(doc => doc.getAllReferences)
    val updatedReferences = documentInfo.getAllReferences.map(sysmlReference => addAbstractions(sysmlReference, landoReferences.toSet))

    SysMLDocumentInfo(
      documentInfo.documentName,
      documentInfo.filePath,
      updatedReferences.filter(_.referenceType == ReferenceType.System),
      updatedReferences.filter(_.referenceType == ReferenceType.SubSystem),
      updatedReferences.filter(_.referenceType == ReferenceType.Connection),
      updatedReferences.filter(_.referenceType == ReferenceType.Scenario),
      updatedReferences.filter(_.referenceType == ReferenceType.Requirement),
      updatedReferences.filter(_.referenceType == ReferenceType.Event),
      updatedReferences.filter(_.referenceType == ReferenceType.Import),
      updatedReferences.filter(_.referenceType == ReferenceType.View),
      updatedReferences.filter(_.referenceType == ReferenceType.Component)
    )
  } ensuring ((resDoc: SysMLDocumentInfo) => resDoc.documentName == documentInfo.documentName
    && resDoc.filePath == documentInfo.filePath
    && resDoc.getAllReferences.count(_.referenceType == ReferenceType.Requirement) == documentInfo.getAllReferences.count(_.referenceType == ReferenceType.Requirement)
    && resDoc.getAllReferences.count(_.referenceType == ReferenceType.Event) == documentInfo.getAllReferences.count(_.referenceType == ReferenceType.Event)
    && resDoc.getAllReferences.count(_.referenceType == ReferenceType.Scenario) == documentInfo.getAllReferences.count(_.referenceType == ReferenceType.Scenario)
    && resDoc.getAllReferences.count(_.referenceType == ReferenceType.Component) == documentInfo.getAllReferences.count(_.referenceType == ReferenceType.Component)
    && resDoc.getAllReferences.size == documentInfo.getAllReferences.size
    )

  override def addSpecializationsToDocument(documentInfo: DocumentInfo, cryptolDocuments: Array[DocumentInfo]): DocumentInfo = {
    require(cryptolDocuments.forall(_.documentType == DocumentType.Cryptol))
    require(documentInfo.documentType == DocumentType.SysML)

    val cryptolReferences = cryptolDocuments.flatMap(doc => doc.getAllReferences.filter(ref => ref.specializes.nonEmpty))
    val updatedReferences = documentInfo.getAllReferences.map(sysmlRef => addSpecializations(sysmlRef, cryptolReferences.toSet))

    SysMLDocumentInfo(
      documentInfo.documentName,
      documentInfo.filePath,
      updatedReferences.filter(_.referenceType == ReferenceType.System),
      updatedReferences.filter(_.referenceType == ReferenceType.SubSystem),
      updatedReferences.filter(_.referenceType == ReferenceType.Connection),
      updatedReferences.filter(_.referenceType == ReferenceType.Scenario),
      updatedReferences.filter(_.referenceType == ReferenceType.Requirement),
      updatedReferences.filter(_.referenceType == ReferenceType.Event),
      updatedReferences.filter(_.referenceType == ReferenceType.Import),
      updatedReferences.filter(_.referenceType == ReferenceType.View),
      updatedReferences.filter(_.referenceType == ReferenceType.Component)
    )
  } ensuring ((resDoc: SysMLDocumentInfo) => resDoc.documentName == documentInfo.documentName
    && resDoc.filePath == documentInfo.filePath
    && resDoc.getAllReferences.count(_.referenceType == ReferenceType.Requirement) == documentInfo.getAllReferences.count(_.referenceType == ReferenceType.Requirement)
    && resDoc.getAllReferences.count(_.referenceType == ReferenceType.Event) == documentInfo.getAllReferences.count(_.referenceType == ReferenceType.Event)
    && resDoc.getAllReferences.count(_.referenceType == ReferenceType.Scenario) == documentInfo.getAllReferences.count(_.referenceType == ReferenceType.Scenario)
    && resDoc.getAllReferences.size == documentInfo.getAllReferences.size
    )
}
