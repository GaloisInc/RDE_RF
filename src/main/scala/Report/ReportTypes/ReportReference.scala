package Report.ReportTypes

import DocumentEnrichers.DocumentEnricher
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
                          ) {

  lazy val documentNamesToPaths: Map[String, String] = {
    val allDocuments = landoDocuments ++ lobotDocuments ++ sysmlDocuments ++ cryptolDocuments ++ sawDocuments ++ bsvDocuments ++ svDocuments ++ cDocuments
    allDocuments.map(d => d.documentName -> d.filePath).toMap
  }

  lazy val getAllReferences: Array[DocReference] = {
    val allDocuments = landoDocuments ++ lobotDocuments ++ sysmlDocuments ++ cryptolDocuments ++ sawDocuments ++ bsvDocuments ++ svDocuments ++ cDocuments
    allDocuments.flatMap(_.getAllReferences)
  }

  def updateDocumentByName(documentName: String, docRef: DocReference): Documents = {
    val allDocuments = landoDocuments ++ lobotDocuments ++ sysmlDocuments ++ cryptolDocuments ++ sawDocuments ++ bsvDocuments ++ svDocuments ++ cDocuments
    allDocuments.find(_.documentName == documentName) match {
      case Some(documentInfo) => documentInfo.documentType match {
        case Types.DocumentType.Lando => copy(landoDocuments = landoDocuments.map(d => if (d.documentName == documentName) d.updateReference(docRef) else d))
        case Types.DocumentType.Lobot => copy(lobotDocuments = lobotDocuments.map(d => if (d.documentName == documentName) d.updateReference(docRef) else d))
        case Types.DocumentType.SysML => copy(sysmlDocuments = sysmlDocuments.map(d => if (d.documentName == documentName) d.updateReference(docRef) else d))
        case Types.DocumentType.Cryptol => copy(cryptolDocuments = cryptolDocuments.map(d => if (d.documentName == documentName) d.updateReference(docRef) else d))
        case Types.DocumentType.Saw => copy(sawDocuments = sawDocuments.map(d => if (d.documentName == documentName) d.updateReference(docRef) else d))
        case Types.DocumentType.SV => copy(svDocuments = svDocuments.map(d => if (d.documentName == documentName) d.updateReference(docRef) else d))
        case Types.DocumentType.BSV => copy(bsvDocuments = bsvDocuments.map(d => if (d.documentName == documentName) d.updateReference(docRef) else d))
        case Types.DocumentType.C => copy(cDocuments = cDocuments.map(d => if (d.documentName == documentName) d.updateReference(docRef) else d))
      }
      case None => throw new IllegalArgumentException(s"Document $documentName not found")
    }
  }

  def fromDocuments(documents: Array[DocumentInfo[_]]): Documents = {
    val landoDocuments = documents.collect { case d: LandoDocumentInfo => d }
    val lobotDocuments = documents.collect { case d: LobotDocumentInfo => d }
    val sysmlDocuments = documents.collect { case d: SysMLDocumentInfo => d }
    val cryptolDocuments = documents.collect { case d: CryptolDocumentInfo => d }
    val sawDocuments = documents.collect { case d: SawDocumentInfo => d }
    val bsvDocuments = documents.collect { case d: BSVDocumentInfo => d }
    val svDocuments = documents.collect { case d: SVDocumentInfo => d }
    val cDocuments = documents.collect { case d: CDocumentInfo => d }
    Documents(landoDocuments, lobotDocuments, sysmlDocuments, cryptolDocuments, sawDocuments, bsvDocuments, svDocuments, cDocuments)
  }
}

final case class ReportReference(
                                  title: String,
                                  author: String,
                                  folder: String,
                                  documents: Documents,
                                  layout: PaperLayout = PaperLayout.A4,
                                ) {


  require(title.nonEmpty, "title must not be empty")
  require(folder.nonEmpty, "folder must not be empty")
  require(author.nonEmpty, "author must not be empty")
  //require(allDocuments.nonEmpty, "At least one document must be provided")
  //require(FileSpecs.allFilesExist(allDocuments.map(_.filePath).toSet), "All documents must exist")
  lazy val allDocumentNamesToPaths: Map[String, String] = {
    documents.documentNamesToPaths
  }

  lazy val getAllReferences: Array[DocReference] = {
    documents.getAllReferences
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

  def moveFiles(folder: String): ReportReference = {
    val lando = documents.landoDocuments.map(d => d.moveFile(folder))
    val lobot = documents.lobotDocuments.map(d => d.moveFile(folder))
    val sysml = documents.sysmlDocuments.map(d => d.moveFile(folder))
    val cryptol = documents.cryptolDocuments.map(d => d.moveFile(folder))
    val saw = documents.sawDocuments.map(d => d.moveFile(folder))
    val bsv = documents.bsvDocuments.map(d => d.moveFile(folder))
    val sv = documents.svDocuments.map(d => d.moveFile(folder))
    val c = documents.cDocuments.map(d => d.moveFile(folder))
    copy(documents = Documents(lando, lobot, sysml, cryptol, saw, bsv, sv, c))
  }
}
