package Referencer

import Types.DocumentInfos.{DocumentInfo, DocumentInfoCompare, SVDocumentInfo}
import Types.DocumentType

class SystemVerilogReferencer extends Referencer {
  override def addRefinementRelations(documentToExtend: DocumentInfo, abstractDocuments: Array[DocumentInfo], refinedDocuments: Array[DocumentInfo]): DocumentInfo = {
    require(abstractDocuments.forall(_.documentType == DocumentType.Cryptol), "SystemVerilogReferencer: abstract documents must be Cryptol")
    require(documentToExtend.documentType == DocumentType.SV, "SystemVerilogReferencer: document to extend must be SystemVerilog")
    addAbstractionsToDocument(documentToExtend, abstractDocuments)
    addSpecializationsToDocument(documentToExtend, refinedDocuments)
  } ensuring ((resDoc: DocumentInfo) => DocumentInfoCompare.compare(resDoc, documentToExtend))

  override def addAbstractionsToDocument(refinedDocument: DocumentInfo, documentsBeingRefined: Array[DocumentInfo]): DocumentInfo = {
    logger.info("Adding abstractions to document " + refinedDocument.documentName)
    require(refinedDocument.documentType == DocumentType.SV, "SystemVerilogReferencer: document to extend must be SystemVerilog")
    require(documentsBeingRefined.forall(_.documentType == DocumentType.Cryptol), "SystemVerilogReferencer: abstract documents must be Cryptol")

    val cryptolReferences = documentsBeingRefined.flatMap(_.getAllReferences)
    val updatedReferences = refinedDocument.getAllReferences.map(svReference => findRefinementRelation(svReference, cryptolReferences.toSet))

    new SVDocumentInfo(refinedDocument.documentName, refinedDocument.filePath, updatedReferences)

  } ensuring ((resDoc: DocumentInfo) => DocumentInfoCompare.compare(resDoc, refinedDocument))

  override def addSpecializationsToDocument(documentInfo: DocumentInfo, refinedDocuments: Array[DocumentInfo]): DocumentInfo = {
    require(documentInfo.documentType == DocumentType.SV, "SystemVerilogReferencer: document to extend must be SystemVerilog")
    require(refinedDocuments.isEmpty, "SystemVerilogReferencer: refined documents must be empty")
    documentInfo
  } ensuring((resDoc: DocumentInfo) => resDoc == documentInfo, "No specializations allowed in SystemVerilog")
}


