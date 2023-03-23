package Report.ReportTypes

import Report.PaperLayout.PaperLayout
import Report.{DocumentationDocument, PaperLayout, RefinementDocument}
import Types.DocReference.DocReference
import Types.DocumentInfos._
import scala.collection.parallel.CollectionConverters._

final case class Documents(
                            landoDocuments: Array[LandoDocumentInfo],
                            lobotDocuments: Array[LobotDocumentInfo],
                            sysmlDocuments: Array[SysMLDocumentInfo],
                            cryptolDocuments: Array[CryptolDocumentInfo],
                            sawDocuments: Array[SawDocumentInfo],
                            bsvDocuments: Array[BSVDocumentInfo],
                            svDocuments: Array[SVDocumentInfo],
                            cDocuments: Array[CDocumentInfo],
                            fretDocuments: Array[FretDocument],
                          ) {

  lazy val numberOfDocuments: Int = landoDocuments.length + lobotDocuments.length + sysmlDocuments.length + cryptolDocuments.length + sawDocuments.length + bsvDocuments.length + svDocuments.length + cDocuments.length + fretDocuments.length
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
        case Types.DocumentType.Fret => copy(fretDocuments = fretDocuments.map(d => if (d.documentName == documentName) d.updateReference(docRef) else d))
        case _ => throw new IllegalArgumentException(s"Document $documentName has an invalid document type")
      }
      case None => throw new IllegalArgumentException(s"Document $documentName not found")
    }
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

  private lazy val allDocumentNamesToPaths: Map[String, String] = {
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
    val lando = documents.landoDocuments.par.map(d => d.moveFile(folder))
    val lobot = documents.lobotDocuments.par.map(d => d.moveFile(folder))
    val sysml = documents.sysmlDocuments.par.map(d => d.moveFile(folder))
    val cryptol = documents.cryptolDocuments.par.map(d => d.moveFile(folder))
    val saw = documents.sawDocuments.par.map(d => d.moveFile(folder))
    val bsv = documents.bsvDocuments.par.map(d => d.moveFile(folder))
    val sv = documents.svDocuments.par.map(d => d.moveFile(folder))
    val c = documents.cDocuments.par.map(d => d.moveFile(folder))
    val fret = documents.fretDocuments.par.map(d => d.moveFile(folder))
    copy(documents = Documents(lando.toArray, lobot.toArray, sysml.toArray, cryptol.toArray, saw.toArray, bsv.toArray, sv.toArray, c.toArray, fret.toArray))
  }

  def buildDocumentationReport: String = {
    val document = DocumentationDocument(title,
      author,
      folder,
      documents.landoDocuments,
      documents.lobotDocuments,
      documents.fretDocuments,
      documents.sysmlDocuments,
      documents.cryptolDocuments,
      documents.sawDocuments,
      documents.svDocuments,
      documents.bsvDocuments,
      documents.cDocuments,
      layout)
    document.compile
  }

  def buildRefinementReport: String = {
    val document = RefinementDocument(title,
      author,
      layout,
      folder,
      getRefinedReferences,
      getNonRefinedReferences,
      allDocumentNamesToPaths
    )
    document.compile
  }
}
