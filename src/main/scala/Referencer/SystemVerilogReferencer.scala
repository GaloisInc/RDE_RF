package Referencer

import Types.DocumentInfos.{DocumentInfo, DocumentInfoCompare}
import Types.DocumentType

class SystemVerilogReferencer extends Referencer {
  override def addRefinementRelations(documentToExtend: DocumentInfo, abstractDocuments: Array[DocumentInfo], refinedDocuments: Array[DocumentInfo]): DocumentInfo = {
    require(abstractDocuments.forall(_.documentType == DocumentType.Cryptol))
    require(documentToExtend.documentType == DocumentType.SV)
    addAbstractionsToDocument(documentToExtend, abstractDocuments)
    addSpecializationsToDocument(documentToExtend, refinedDocuments)
  } ensuring ((resDoc: DocumentInfo) => DocumentInfoCompare.compare(resDoc, documentToExtend))

  override def addAbstractionsToDocument(refinedDocument: DocumentInfo, documentsBeingRefined: Array[DocumentInfo]): DocumentInfo = {
    refinedDocument
  } ensuring ((resDoc: DocumentInfo) => DocumentInfoCompare.compare(resDoc, refinedDocument))

  override def addSpecializationsToDocument(documentInfo: DocumentInfo, refinedDocuments: Array[DocumentInfo]): DocumentInfo = {
    documentInfo
  } ensuring ((resDoc: DocumentInfo) => resDoc == documentInfo)
}


