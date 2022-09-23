package Referencer

import Types.DocumentInfos.{BSVDocumentInfo, DocumentInfo, DocumentInfoCompare}
import Types.{DocumentType, ReferenceType}

class BlueSpecReferencer extends Referencer {
  override def addRefinementRelations(documentToExtend: DocumentInfo, abstractDocuments: Array[DocumentInfo], refinedDocuments: Array[DocumentInfo]): DocumentInfo = {
    require(abstractDocuments.forall(_.documentType == DocumentType.Cryptol), "All abstract documents must be Cryptol")
    require(documentToExtend.documentType == DocumentType.BSV, "Document to extend must be BSV")
    require(refinedDocuments.isEmpty, "No refined documents allowed")

    logger.info("Adding refinement relations to " + documentToExtend.documentName + " from " + abstractDocuments.map(_.documentName).mkString(", "))
    addAbstractionsToDocument(documentToExtend, abstractDocuments)
    addSpecializationsToDocument(documentToExtend, refinedDocuments)
  } ensuring ((resDoc: DocumentInfo) => DocumentInfoCompare.compare(resDoc, documentToExtend))

  override def addAbstractionsToDocument(refinedDocument: DocumentInfo, documentsBeingRefined: Array[DocumentInfo]): DocumentInfo = {
    require(documentsBeingRefined.forall(_.documentType == DocumentType.Cryptol), "All documents being refined must be Cryptol")
    require(refinedDocument.documentType == DocumentType.BSV, "Refined document must be BlueSpec")
    logger.info("Adding abstractions to " + refinedDocument.documentName + " from " + documentsBeingRefined.map(_.documentName).mkString(", "))

    val cryptolReferences = documentsBeingRefined.flatMap(doc => doc.getAllReferences)
    val updatedReferences = refinedDocument.getAllReferences.map(bsvReference => findRefinementRelation(bsvReference, cryptolReferences.toSet))

    new BSVDocumentInfo(
      refinedDocument.documentName,
      refinedDocument.filePath,
      updatedReferences.filter(_.getReferenceType == ReferenceType.System),
      updatedReferences.filter(_.getReferenceType == ReferenceType.SubSystem),
    )
  } ensuring ((resDoc: DocumentInfo) => DocumentInfoCompare.compare(resDoc, refinedDocument))

  override def addSpecializationsToDocument(documentInfo: DocumentInfo, refinedDocuments: Array[DocumentInfo]): DocumentInfo = {
    documentInfo
  } ensuring ((resDoc: DocumentInfo) => resDoc == documentInfo, "No specializations allowed for BlueSpec")
}
