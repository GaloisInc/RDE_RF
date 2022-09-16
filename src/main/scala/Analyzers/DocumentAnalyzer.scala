package Analyzers

import DocumentEnrichers.{CryptolDocumentEnricher, LandoDocumentEnricher, SysMLDocumentEnricher}
import Formatter.LatexFormatter
import Referencer.{CryptolReferencer, LandoReferencer, SysMLReferencer}
import Report.PaperLayout.PaperLayout
import Report.ReportTypes.ReportReference
import Types.DocReference.DocReference
import Types.DocumentInfos.{CryptolDocumentInfo, DocumentInfo, LandoDocumentInfo, SysMLDocumentInfo}
import Utils.{FileUtil, Matcher}

import java.nio.file.Path

final case class LatexDocumentData(
                                    title: String,
                                    folder: String,
                                    layout: PaperLayout,
                                    latexFormatter: LatexFormatter
                                  ) {
  require(title.nonEmpty, "Title must not be empty")
  require(folder.nonEmpty, "Folder must not be empty")
}

object DocumentAnalyzer {
  //Referencers
  private val landoReferencer = new LandoReferencer()
  private val sysMLReferencer = new SysMLReferencer()
  private val cryptolReferencer = new CryptolReferencer()

  def generateReport(filesToAnalyze: Array[String],
                     latexDocumentData: LatexDocumentData,
                     sortFiles: Boolean = true): ReportReference = {
    require(filesToAnalyze.nonEmpty, "No files to analyze")

    if (sortFiles)
      enrichAndSortFiles(filesToAnalyze, latexDocumentData)
    else {
      val enrichedDocuments = enrichDocuments(filesToAnalyze, latexDocumentData.latexFormatter)
      ReportReference(
        latexDocumentData.title,
        latexDocumentData.folder,
        FileUtil.getLandoDocuments(enrichedDocuments).map(_.asInstanceOf[LandoDocumentInfo]),
        FileUtil.getSysMLDocuments(enrichedDocuments).map(_.asInstanceOf[SysMLDocumentInfo]),
        FileUtil.getCryptolDocuments(enrichedDocuments).map(_.asInstanceOf[CryptolDocumentInfo]),
        latexDocumentData.layout
      )
    }
  }

  def enrichAndSortFiles(filesToAnalyze: Array[String], latexDocumentData: LatexDocumentData): ReportReference = {
    require(filesToAnalyze.nonEmpty, "No files to analyze")
    val references = enrichFiles(filesToAnalyze, latexDocumentData)

    val targetFolder = latexDocumentData.folder

    val newLandoFiles = references.landoDocuments.map(doc => {
      val destinationPath = Path.of(targetFolder, "decoratedLando").toString
      val filePath = FileUtil.moveRenameFile(doc.filePath, destinationPath)
      doc.copy(filePath = filePath)
    })

    val newSysMLFiles = references.sysmlDocuments.map(doc => {
      val destinationPath = Path.of(targetFolder, "decoratedSysML").toString
      val filePath = FileUtil.moveRenameFile(doc.filePath, destinationPath)
      doc.copy(filePath = filePath)
    })

    val newCryptolFiles = references.cryptolDocuments.map(doc => {
      val destinationPath = Path.of(targetFolder, "decoratedCryptol").toString
      val filePath = FileUtil.moveRenameFile(doc.filePath, destinationPath)
      doc.copy(filePath = filePath)
    })

    references.copy(landoDocuments = newLandoFiles, sysmlDocuments = newSysMLFiles, cryptolDocuments = newCryptolFiles)
  }

  def enrichFiles(filesToAnalyze: Array[String], latexDocumentData: LatexDocumentData): ReportReference = {
    require(filesToAnalyze.nonEmpty, "No files to analyze")
    val formatter: LatexFormatter = latexDocumentData.latexFormatter
    val landoAnalyzer = new LandoDocumentEnricher(formatter)
    val sysMLAnalyzer = new SysMLDocumentEnricher(formatter)
    val cryptolAnalyzer = new CryptolDocumentEnricher(formatter)

    val enrichedDocuments = enrichDocuments(filesToAnalyze, formatter)

    val enrichedLandoDocuments = FileUtil.getLandoDocuments(enrichedDocuments)
    val enrichedSysMLDocuments = FileUtil.getSysMLDocuments(enrichedDocuments)
    val enrichedCryptolDocuments = FileUtil.getCryptolDocuments(enrichedDocuments)

    assert(enrichedCryptolDocuments.intersect(enrichedSysMLDocuments).isEmpty)
    assert(enrichedLandoDocuments.intersect(enrichedSysMLDocuments).isEmpty)
    assert(enrichedCryptolDocuments.intersect(enrichedLandoDocuments).isEmpty)

    val decoratedLando = enrichedLandoDocuments.map(doc => {
      val filePath = landoAnalyzer.decorateFile(doc)
      doc.asInstanceOf[LandoDocumentInfo].copy(filePath = filePath)
    })
    val decoratedSysML = enrichedSysMLDocuments.map(doc => {
      val filePath = sysMLAnalyzer.decorateFile(doc)
      doc.asInstanceOf[SysMLDocumentInfo].copy(filePath = filePath)
    })
    val decoratedCryptol = enrichedCryptolDocuments.map(doc => {
      val filePath = cryptolAnalyzer.decorateFile(doc)
      doc.asInstanceOf[CryptolDocumentInfo].copy(filePath = filePath)
    })

    ReportReference(latexDocumentData.title, latexDocumentData.folder, decoratedLando, decoratedSysML, decoratedCryptol, latexDocumentData.layout)
  } ensuring ((res: ReportReference) => res.cryptolDocuments.length + res.sysmlDocuments.length + res.landoDocuments.length == filesToAnalyze.length)

  def enrichDocuments(filesToAnalyze: Array[String], formatter: LatexFormatter): Array[DocumentInfo] = {
    require(filesToAnalyze.nonEmpty, "No files to analyze")
    val landoAnalyzer = new LandoDocumentEnricher(formatter)
    val sysmlAnalyzer = new SysMLDocumentEnricher(formatter)
    val cryptolAnalyzer = new CryptolDocumentEnricher(formatter)

    val landoFilesToAnalyse = filesToAnalyze.filter(file => FileUtil.getFileType(file).equals("lando"))
    val sysmlFilesToAnalyse = filesToAnalyze.filter(file => FileUtil.getFileType(file).equals("sysml"))
    val cryptolFilesToAnalyse = filesToAnalyze.filter(file => FileUtil.getFileType(file).equals("cry"))

    val landoDocuments = landoFilesToAnalyse.map(landoAnalyzer.parseDocument)
    val sysMLDocuments = sysmlFilesToAnalyse.map(sysmlAnalyzer.parseDocument)
    val cryptolDocuments = cryptolFilesToAnalyse.map(cryptolAnalyzer.parseDocument)

    assert(landoDocuments.intersect(sysMLDocuments).toSet == Set.empty)
    assert(landoDocuments.intersect(cryptolDocuments).toSet == Set.empty)
    assert(sysMLDocuments.intersect(cryptolDocuments).toSet == Set.empty)
    //Ensuring Unique references/labels
    assert(landoDocuments.flatMap(_.getAllReferences.map(_.getLabelText)).toSet.size == landoDocuments.flatMap(_.getAllReferences.map(_.getLabelText)).length)
    assert(cryptolDocuments.flatMap(_.getAllReferences.map(_.getLabelText)).toSet.size == cryptolDocuments.flatMap(_.getAllReferences.map(_.getLabelText)).length)
    assert(sysMLDocuments.flatMap(_.getAllReferences.map(_.getLabelText)).toSet.size == sysMLDocuments.flatMap(_.getAllReferences.map(_.getLabelText)).length)

    val enrichedCryptolDocuments = cryptolDocuments.map(doc => cryptolReferencer.addRefinementRelations(doc, sysMLDocuments.map(_.asInstanceOf[DocumentInfo]), Array.empty[DocumentInfo]))
    val enrichedSysMLDocuments = sysMLDocuments.map(doc => sysMLReferencer.addRefinementRelations(doc, landoDocuments.map(_.asInstanceOf[DocumentInfo]), enrichedCryptolDocuments))
    val enrichedLandoDocuments = landoDocuments.map(doc => landoReferencer.addRefinementRelations(doc, Array.empty[DocumentInfo], enrichedSysMLDocuments))

    enrichedLandoDocuments.foreach(doc => addReferences(doc, enrichedLandoDocuments.flatMap(_.getAllReferences).toSet))
    enrichedSysMLDocuments.foreach(doc => addReferences(doc, enrichedSysMLDocuments.flatMap(_.getAllReferences).toSet))
    enrichedCryptolDocuments.foreach(doc => addReferences(doc, enrichedCryptolDocuments.flatMap(_.getAllReferences).toSet))

    enrichedLandoDocuments ++ enrichedSysMLDocuments ++ enrichedCryptolDocuments
  } ensuring ((res: Array[DocumentInfo]) => res.length == filesToAnalyze.length)


  def addReferences(document: DocumentInfo, references: Set[DocReference]): Unit = {
    val referencesToUpdate: Set[DocReference] = document.getAllReferences.filter(ref => ref.isReferencingAnything)
    referencesToUpdate.foreach(reference => {
      val potentialReferences: Map[String, DocReference] =
        references.flatMap(r => {
          reference.getStringReferences.get
            .map(ref => Matcher.getReferenceName(ref.name, r.getReferenceName))
            .filter(_.isDefined)
            .map(_.get)
            .map(ref => (ref, r))
        }).toMap
      potentialReferences.foreach(ref => reference.addReference(ref))
    })
  }
}
