package Referencer

import Types.{CryptolDocumentInfo, DocumentInfo, DocumentType, LandoDocumentInfo, ReferenceType, SysMLDocumentInfo}

class LandoReferencer extends Referencer {
  def addSpecializationAndAbstract(documentToExtend: DocumentInfo, abstractDocuments: Array[DocumentInfo], specializedDocuments: Array[DocumentInfo]): DocumentInfo = {
    require(specializedDocuments.forall(_.documentType == DocumentType.SysML))
    require(documentToExtend.documentType == DocumentType.Lando)
    addSpecializationsToDocument(documentToExtend, specializedDocuments)
  }

  def addAbstractionsToDocument(specializedDocument: DocumentInfo, abstractDocument: Array[DocumentInfo]): DocumentInfo = {
    specializedDocument
  } ensuring ((res: DocumentInfo) => res == specializedDocument)

  def addSpecializationsToDocument(documentInfo: DocumentInfo, sysMLDocuments: Array[DocumentInfo]): DocumentInfo = {
    require(sysMLDocuments.forall(_.documentType == DocumentType.SysML))
    require(documentInfo.documentType == DocumentType.Lando)

    val sysmlReferences = sysMLDocuments.flatMap(doc => doc.getAllReferences().filter(ref => ref.specializes.nonEmpty))
    val updatedReferences = documentInfo.getAllReferences().map(landoRef => addSpecializations(landoRef, sysmlReferences.toSet))

    val componentSet = Set(ReferenceType.Component, ReferenceType.SubSystem, ReferenceType.System)
    LandoDocumentInfo(
      documentInfo.documentName,
      documentInfo.filePath,
      updatedReferences.filter(ref => componentSet.contains(ref.referenceType)),
      documentInfo.getRelations(),
      updatedReferences.filter(_.referenceType == ReferenceType.Event),
      updatedReferences.filter(_.referenceType == ReferenceType.Requirement),
      updatedReferences.filter(_.referenceType == ReferenceType.Scenario),
    )
  } ensuring ((resDoc: LandoDocumentInfo) => resDoc.documentName == documentInfo.documentName
    && resDoc.filePath == documentInfo.filePath
    && resDoc.getRelations().size == documentInfo.getRelations().size
    && resDoc.getAllReferences().count(_.referenceType == ReferenceType.Requirement) == documentInfo.getAllReferences().count(_.referenceType == ReferenceType.Requirement)
    && resDoc.getAllReferences().count(_.referenceType == ReferenceType.Event) == documentInfo.getAllReferences().count(_.referenceType == ReferenceType.Event)
    && resDoc.getAllReferences().count(_.referenceType == ReferenceType.Scenario) == documentInfo.getAllReferences().count(_.referenceType == ReferenceType.Scenario)
    && resDoc.getAllReferences().size == documentInfo.getAllReferences().size
    )
}






