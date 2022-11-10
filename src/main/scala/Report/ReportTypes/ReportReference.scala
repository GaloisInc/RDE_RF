package Report.ReportTypes

import Report.PaperLayout.PaperLayout
import Specs.FileSpecs
import Types.DocReference.DocReference
import Types.DocumentInfos._
import Utils.FileUtil

case class ReportReference(title: String,
                           folder: String,
                           landoDocuments: Array[LandoDocumentInfo],
                           lobotDocuments: Array[LobotDocumentInfo],
                           fretDocuments: Array[FRETDocumentInfo],
                           sysmlDocuments: Array[SysMLDocumentInfo],
                           cryptolDocuments: Array[CryptolDocumentInfo],
                           sawDocuments: Array[SawDocumentInfo],
                           bsvDocuments: Array[BSVDocumentInfo],
                           svDocuments: Array[SVDocumentInfo],
                           cDocuments : Array[CDocumentInfo],
                           layout: PaperLayout,
                          ) {
  require(title.nonEmpty, "title must not be empty")
  require(folder.nonEmpty, "folder must not be empty")
  require(allDocuments.nonEmpty, "At least one document must be provided")
  require(FileSpecs.allFilesExist(allDocuments.map(_.filePath).toSet), "All documents must exist")

  lazy val allDocuments: Array[DocumentInfo] = landoDocuments ++ sysmlDocuments ++ cryptolDocuments ++ bsvDocuments ++ svDocuments ++ cDocuments ++ sawDocuments ++ fretDocuments ++ lobotDocuments

  def updateDocument(documentInfo: DocumentInfo, docRef: DocReference) : ReportReference = {
    documentInfo.documentType match {
      case Types.DocumentType.Lando => copy(landoDocuments = landoDocuments.map(d => if (d.documentName == documentInfo.documentName) d.updateReference(docRef).asInstanceOf[LandoDocumentInfo] else d))
      case Types.DocumentType.SysML => copy(sysmlDocuments = sysmlDocuments.map(d => if (d.documentName == documentInfo.documentName) d.updateReference(docRef).asInstanceOf[SysMLDocumentInfo] else d))
      case Types.DocumentType.Cryptol => copy(cryptolDocuments = cryptolDocuments.map(d => if (d.documentName == documentInfo.documentName) d.updateReference(docRef).asInstanceOf[CryptolDocumentInfo] else d))
      case Types.DocumentType.BSV => copy(bsvDocuments = bsvDocuments.map(d => if (d.documentName == documentInfo.documentName) d.updateReference(docRef).asInstanceOf[BSVDocumentInfo] else d))
      case Types.DocumentType.SV => copy(svDocuments = svDocuments.map(d => if (d.documentName == documentInfo.documentName) d.updateReference(docRef).asInstanceOf[SVDocumentInfo] else d))
      case Types.DocumentType.C => copy(cDocuments = cDocuments.map(d => if (d.documentName == documentInfo.documentName) d.updateReference(docRef).asInstanceOf[CDocumentInfo] else d))
      case Types.DocumentType.Saw => copy(sawDocuments = sawDocuments.map(d => if (d.documentName == documentInfo.documentName) d.updateReference(docRef).asInstanceOf[SawDocumentInfo] else d))
      case Types.DocumentType.Lobot => copy(lobotDocuments = lobotDocuments.map(d => if (d.documentName == documentInfo.documentName) d.updateReference(docRef).asInstanceOf[LobotDocumentInfo] else d))
      case Types.DocumentType.FRET => copy(fretDocuments = fretDocuments.map(d => if (d.documentName == documentInfo.documentName) d.updateReference(docRef).asInstanceOf[FRETDocumentInfo] else d))
      case _ => throw new IllegalArgumentException(s"Unknown document type ${documentInfo.documentType}")
    }
  }

  def updateDocumentByName(documentName: String, docRef: DocReference) : ReportReference = {
    allDocuments.find(_.documentName == documentName) match {
      case Some(documentInfo) => updateDocument(documentInfo, docRef)
      case None => throw new IllegalArgumentException(s"Document $documentName not found")
    }
  }

    def allDocumentNamesToPaths: Map[String, String] = {
    val docMap = allDocuments.map(doc => doc.documentName -> doc.filePath).toMap
    docMap
  } //ensuring(_.size == allDocuments.length, "All documents must be present in the map")

  lazy val getAllReferences: Array[DocReference] = allDocuments.flatMap(_.getAllReferences)

  lazy val getNonRefinedReferences: Set[DocReference] = {
    val allReferences = getAllReferences
    val notReferenced = allReferences.filterNot(_.isInRefinementChain)
    notReferenced.toSet
  } //ensuring((refs: Set[DocReference]) => refs.subsetOf(getAllReferences.toSet), "All non-refined references must be in the set of all references")

  lazy val getRefinedReferences: Set[DocReference] = {
    val allReferences = getAllReferences
    val referenced = allReferences.filter(_.isInRefinementChain)
    referenced.toSet
  } //ensuring((refs: Set[DocReference]) => refs.subsetOf(getAllReferences.toSet), "All refined references must be in the set of all references")


}
