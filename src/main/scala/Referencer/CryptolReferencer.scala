package Referencer

import Types.*
import Types.DocumentInfos.{DocumentInfo, CryptolDocumentInfo}

class CryptolReferencer extends Referencer {
  override def addSpecializationAndAbstract(documentToExtend: DocumentInfo, abstractDocuments: Array[DocumentInfo], specializedDocuments: Array[DocumentInfo]): DocumentInfo = {
    require(abstractDocuments.forall(_.documentType == DocumentType.SysML))
    require(documentToExtend.documentType == DocumentType.Cryptol)
    addAbstractionsToDocument(documentToExtend, abstractDocuments)
  }

  override def addAbstractionsToDocument(documentInfo: DocumentInfo, sysMlDocuments: Array[DocumentInfo]): DocumentInfo = {
    require(sysMlDocuments.forall(_.documentType == DocumentType.SysML))
    require(documentInfo.documentType == DocumentType.Cryptol)

    val sysmlReferences = sysMlDocuments.flatMap(doc => doc.getAllReferences)
    val updatedReferences = documentInfo.getAllReferences.map(reference => addAbstractions(reference, sysmlReferences.toSet))

    CryptolDocumentInfo(
      documentInfo.documentName,
      documentInfo.filePath,
      updatedReferences.filter(_.referenceType == ReferenceType.Import),
      updatedReferences.filter(_.referenceType == ReferenceType.Type),
      updatedReferences.filter(_.referenceType == ReferenceType.Event),
      updatedReferences.filter(_.referenceType == ReferenceType.Requirement),
    )
  } ensuring ((resDoc: DocumentInfo) => resDoc.documentName == documentInfo.documentName
    && resDoc.filePath == documentInfo.filePath
    )

  override def addSpecializationsToDocument(documentInfo: DocumentInfo, specializedDocuments: Array[DocumentInfo]): DocumentInfo = {
    documentInfo
  }
}
