package Report.ReportTypes

import Report.PaperLayout
import Report.PaperLayout.PaperLayout
import Types.DocReference.DocReference
import Types.DocumentInfos._


final case class Documents(
                            landoDocuments: Array[LandoDocumentInfo],
                            lobotDocuments: Array[LobotDocumentInfo],
                            sysmlDocuments: Array[SysMLDocumentInfo],
                            cryptolDocuments: Array[CryptolDocumentInfo],
                            sawDocuments: Array[SawDocumentInfo],
                            bsvDocuments: Array[BSVDocumentInfo],
                            svDocuments: Array[SVDocumentInfo],
                            cDocuments: Array[CDocumentInfo],
                          )

final case class ReportReference(
                                     title: String,
                                     author: String,
                                     folder: String,
                                     landoDocuments: Array[LandoDocumentInfo],
                                     lobotDocuments: Array[LobotDocumentInfo],
                                     sysmlDocuments: Array[SysMLDocumentInfo],
                                     cryptolDocuments: Array[CryptolDocumentInfo],
                                     sawDocuments: Array[SawDocumentInfo],
                                     bsvDocuments: Array[BSVDocumentInfo],
                                     svDocuments: Array[SVDocumentInfo],
                                     cDocuments: Array[CDocumentInfo],
                                     layout: PaperLayout = PaperLayout.A4,
                                   ) {


  require(title.nonEmpty, "title must not be empty")
  require(folder.nonEmpty, "folder must not be empty")
  require(author.nonEmpty, "author must not be empty")
  //require(allDocuments.nonEmpty, "At least one document must be provided")
  //require(FileSpecs.allFilesExist(allDocuments.map(_.filePath).toSet), "All documents must exist")

  private def updateDocument[T <: DocumentInfo[T]](documentInfo: T, docRef: DocReference): ReportReference = {
    documentInfo.documentType match {
      case Types.DocumentType.Lando => copy(landoDocuments = landoDocuments.map(d => if (d.documentName == documentInfo.documentName) d.updateReference(docRef) else d))
      case Types.DocumentType.SysML => copy(sysmlDocuments = sysmlDocuments.map(d => if (d.documentName == documentInfo.documentName) d.updateReference(docRef) else d))
      case Types.DocumentType.Cryptol => copy(cryptolDocuments = cryptolDocuments.map(d => if (d.documentName == documentInfo.documentName) d.updateReference(docRef) else d))
      case Types.DocumentType.BSV => copy(bsvDocuments = bsvDocuments.map(d => if (d.documentName == documentInfo.documentName) d.updateReference(docRef) else d))
      case Types.DocumentType.SV => copy(svDocuments = svDocuments.map(d => if (d.documentName == documentInfo.documentName) d.updateReference(docRef) else d))
      case Types.DocumentType.C => copy(cDocuments = cDocuments.map(d => if (d.documentName == documentInfo.documentName) d.updateReference(docRef) else d))
      case Types.DocumentType.Saw => copy(sawDocuments = sawDocuments.map(d => if (d.documentName == documentInfo.documentName) d.updateReference(docRef) else d))
      case _ => throw new IllegalArgumentException(s"Unknown document type ${documentInfo.documentType}")
    }
  }

  /*
  def updateDocumentByName(documentName: String, docRef: DocReference): ReportReference[T] = {
    allDocuments.find(_.documentName == documentName) match {
      case Some(documentInfo) => updateDocument(documentInfo, docRef)
      case None => throw new IllegalArgumentException(s"Document $documentName not found")
    }
  }
   */

  def allDocumentNamesToPaths: Map[String, String] = {
    val landoDocumentMap = landoDocuments.map(doc => doc.documentName -> doc.filePath).toMap
    val lobotDocumentMap = lobotDocuments.map(doc => doc.documentName -> doc.filePath).toMap
    val sysmlDocumentMap = sysmlDocuments.map(doc => doc.documentName -> doc.filePath).toMap
    val cryptolDocumentMap = cryptolDocuments.map(doc => doc.documentName -> doc.filePath).toMap
    val sawDocumentMap = sawDocuments.map(doc => doc.documentName -> doc.filePath).toMap
    val bsvDocumentMap = bsvDocuments.map(doc => doc.documentName -> doc.filePath).toMap
    val svDocumentMap = svDocuments.map(doc => doc.documentName -> doc.filePath).toMap
    val cDocumentMap = cDocuments.map(doc => doc.documentName -> doc.filePath).toMap
    landoDocumentMap ++ lobotDocumentMap ++ sysmlDocumentMap ++ cryptolDocumentMap ++ sawDocumentMap ++ bsvDocumentMap ++ svDocumentMap ++ cDocumentMap
  } //ensuring(_.size == allDocuments.length, "All documents must be present in the map")

  lazy val getAllReferences: Array[DocReference] = {
    val landoRefs = landoDocuments.flatMap(_.getAllReferences)
    val lobotRefs = lobotDocuments.flatMap(_.getAllReferences)
    val sysmlRefs = sysmlDocuments.flatMap(_.getAllReferences)
    val cryptolRefs = cryptolDocuments.flatMap(_.getAllReferences)
    val sawRefs = sawDocuments.flatMap(_.getAllReferences)
    val bsvRefs = bsvDocuments.flatMap(_.getAllReferences)
    val svRefs = svDocuments.flatMap(_.getAllReferences)
    val cRefs = cDocuments.flatMap(_.getAllReferences)
    landoRefs ++ lobotRefs ++ sysmlRefs ++ cryptolRefs ++ sawRefs ++ bsvRefs ++ svRefs ++ cRefs
  }

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
