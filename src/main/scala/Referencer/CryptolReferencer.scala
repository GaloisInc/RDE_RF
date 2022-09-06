package Referencer

import Types.*
import Types.DocumentInfos.{CryptolDocumentInfo, DocumentInfo, DocumentInfoCompare}

class CryptolReferencer extends Referencer {
  override def addSpecializationAndAbstract(documentToExtend: DocumentInfo, abstractDocuments: Array[DocumentInfo], specializedDocuments: Array[DocumentInfo]): DocumentInfo = {
    require(abstractDocuments.forall(_.documentType == DocumentType.SysML))
    require(documentToExtend.documentType == DocumentType.Cryptol)
    addAbstractionsToDocument(documentToExtend, abstractDocuments)
  } ensuring ((resDoc: DocumentInfo) => DocumentInfoCompare.compare(resDoc, documentToExtend))

  override def addAbstractionsToDocument(documentInfo: DocumentInfo, sysMlDocuments: Array[DocumentInfo]): DocumentInfo = {
    require(sysMlDocuments.forall(_.documentType == DocumentType.SysML))
    require(documentInfo.documentType == DocumentType.Cryptol)

    val sysmlReferences = sysMlDocuments.flatMap(doc => doc.getAllReferences)
    val updatedReferences = documentInfo.getAllReferences.map(reference => addAbstractions(reference, sysmlReferences.toSet))

    CryptolDocumentInfo(
      documentInfo.documentName,
      documentInfo.filePath,
      updatedReferences.filter(_.getReferenceType == ReferenceType.Import),
      updatedReferences.filter(_.getReferenceType == ReferenceType.Type),
      updatedReferences.filter(_.getReferenceType == ReferenceType.Event),
      updatedReferences.filter(_.getReferenceType == ReferenceType.Requirement),
    )
  } ensuring ((resDoc: DocumentInfo) => DocumentInfoCompare.compare(resDoc, documentInfo))

  override def addSpecializationsToDocument(documentInfo: DocumentInfo, specializedDocuments: Array[DocumentInfo]): DocumentInfo = {
    documentInfo
  } ensuring ((resDoc: DocumentInfo) => resDoc == documentInfo)
}
