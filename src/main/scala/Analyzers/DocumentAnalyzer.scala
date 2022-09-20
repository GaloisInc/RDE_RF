package Analyzers

import ConfigParser.RefinementModel
import DocumentEnrichers._
import Formatter.LatexFormatter
import Referencer.{CryptolReferencer, LandoReferencer, SysMLReferencer}
import Report.PaperLayout.PaperLayout
import Report.ReportTypes.ReportReference
import Types.DocReference.DocReference
import Types.DocumentInfos._
import Utils.{FileUtil, Matcher}

import java.nio.file.{Path, Paths}

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
                     explicitRefinements: Set[RefinementModel],
                     sortFiles: Boolean = true): ReportReference = {
    require(filesToAnalyze.nonEmpty, "No files to analyze")

    if (sortFiles) enrichAndMoveFiles(filesToAnalyze, latexDocumentData, explicitRefinements)
    else enrichFiles(filesToAnalyze, latexDocumentData, explicitRefinements)
  }

  def enrichAndMoveFiles(filesToAnalyze: Array[String], latexDocumentData: LatexDocumentData, explicitRefinements: Set[RefinementModel]): ReportReference = {
    require(filesToAnalyze.nonEmpty, "No files to analyze")
    val references = enrichFiles(filesToAnalyze, latexDocumentData, explicitRefinements)
    moveFiles(references)
  }

  def moveFiles(reportReference: ReportReference): ReportReference = {
    val targetFolder = reportReference.folder
    val newLandoFiles = reportReference.landoDocuments.map(doc => {
      val destinationPath = Paths.get(targetFolder, "decoratedLando").toString
      val filePath = FileUtil.moveRenameFile(doc.filePath, destinationPath)
      doc.copy(filePath = filePath)
    })

    val newSysMLFiles = reportReference.sysmlDocuments.map(doc => {
      val destinationPath = Paths.get(targetFolder, "decoratedSysML").toString
      val filePath = FileUtil.moveRenameFile(doc.filePath, destinationPath)
      doc.copy(filePath = filePath)
    })

    val newCryptolFiles = reportReference.cryptolDocuments.map(doc => {
      val destinationPath = Paths.get(targetFolder, "decoratedCryptol").toString
      val filePath = FileUtil.moveRenameFile(doc.filePath, destinationPath)
      doc.copy(filePath = filePath)
    })

    val newBSVFiles = reportReference.bsvDocuments.map(doc => {
      val destinationPath = Paths.get(targetFolder, "decoratedBSV").toString
      val filePath = FileUtil.moveRenameFile(doc.filePath, destinationPath)
      doc.copy(filePath = filePath)
    })

    val newSVFiles = reportReference.svDocuments.map(doc => {
      val destinationPath = Paths.get(targetFolder, "decoratedSV").toString
      val filePath = FileUtil.moveRenameFile(doc.filePath, destinationPath)
      doc.copy(filePath = filePath)
    })

    reportReference.copy(
      landoDocuments = newLandoFiles,
      sysmlDocuments = newSysMLFiles,
      cryptolDocuments = newCryptolFiles,
      bsvDocuments = newBSVFiles,
      svDocuments = newSVFiles)
  }

  def enrichFiles(filesToAnalyze: Array[String], latexDocumentData: LatexDocumentData, explicitReferences : Set[RefinementModel] = Set.empty[RefinementModel]): ReportReference = {
    require(filesToAnalyze.nonEmpty, "No files to analyze")

    val formatter = latexDocumentData.latexFormatter
    val enrichedDocuments = enrichDocuments(filesToAnalyze, formatter)

    val enrichedLandoDocuments = FileUtil.getLandoDocuments(enrichedDocuments)
    val enrichedSysMLDocuments = FileUtil.getSysMLDocuments(enrichedDocuments)
    val enrichedCryptolDocuments = FileUtil.getCryptolDocuments(enrichedDocuments)
    val enrichedBSVDocuments = FileUtil.getBlusSpecDocuments(enrichedDocuments)
    val enrichedSVDocuments = FileUtil.getSystemVerilogDocumetns(enrichedDocuments)

    val report = ReportReference(
      latexDocumentData.title,
      latexDocumentData.folder,
      enrichedLandoDocuments.map(_.asInstanceOf[LandoDocumentInfo]),
      enrichedSysMLDocuments.map(_.asInstanceOf[SysMLDocumentInfo]),
      enrichedCryptolDocuments.map(_.asInstanceOf[CryptolDocumentInfo]),
      enrichedBSVDocuments.map(_.asInstanceOf[BSVDocumentInfo]),
      enrichedSVDocuments.map(_.asInstanceOf[SVDocumentInfo]),
      latexDocumentData.layout
    )

    val updatedReport = addExplicitRefinements(report, explicitReferences)

    enrichReport(updatedReport, formatter)
  } ensuring ((res: ReportReference) => res.allDocuments.length == filesToAnalyze.length)


  def enrichReport(reportReference: ReportReference, formatter: LatexFormatter):ReportReference = {
    val landoAnalyzer = new LandoDocumentEnricher(formatter)
    val sysMLAnalyzer = new SysMLDocumentEnricher(formatter)
    val cryptolAnalyzer = new CryptolDocumentEnricher(formatter)
    val bsvAnalyzer = new BSVDocumentEnricher(formatter)
    val svAnalyzer = new SVDocumentEnricher(formatter)

    val decoratedLando = reportReference.landoDocuments.map(doc => {
      val filePath = landoAnalyzer.decorateFile(doc)
      doc.copy(filePath = filePath)
    })
    val decoratedSysML = reportReference.sysmlDocuments.map(doc => {
      val filePath = sysMLAnalyzer.decorateFile(doc)
      doc.copy(filePath = filePath)
    })
    val decoratedCryptol = reportReference.cryptolDocuments.map(doc => {
      val filePath = cryptolAnalyzer.decorateFile(doc)
      doc.copy(filePath = filePath)
    })
    val decoratedBSV = reportReference.bsvDocuments.map(doc => {
      val filePath = bsvAnalyzer.decorateFile(doc)
      doc.copy(filePath = filePath)
    })
    val decoratedSV = reportReference.svDocuments.map(doc => {
      val filePath = svAnalyzer.decorateFile(doc)
      doc.copy(filePath = filePath)
    })

    reportReference.copy(
      landoDocuments = decoratedLando,
      sysmlDocuments = decoratedSysML,
      cryptolDocuments = decoratedCryptol,
      bsvDocuments = decoratedBSV,
      svDocuments = decoratedSV
    )
  }

  def enrichDocuments(filesToAnalyze: Array[String], formatter: LatexFormatter): Array[DocumentInfo] = {
    require(filesToAnalyze.nonEmpty, "No files to analyze")
    val landoAnalyzer = new LandoDocumentEnricher(formatter)
    val sysmlAnalyzer = new SysMLDocumentEnricher(formatter)
    val cryptolAnalyzer = new CryptolDocumentEnricher(formatter)
    val bsvAnalyzer = new BSVDocumentEnricher(formatter)
    val svAnalyzer = new SVDocumentEnricher(formatter)

    val landoFilesToAnalyse = filesToAnalyze.filter(file => FileUtil.getFileType(file).equals("lando"))
    val sysmlFilesToAnalyse = filesToAnalyze.filter(file => FileUtil.getFileType(file).equals("sysml"))
    val cryptolFilesToAnalyse = filesToAnalyze.filter(file => FileUtil.getFileType(file).equals("cry"))
    val bsvFilesToAnalyse = filesToAnalyze.filter(file => FileUtil.getFileType(file).equals("bsv"))
    val svFilesToAnalyse = filesToAnalyze.filter(file => FileUtil.getFileType(file).equals("sv"))

    val landoDocuments = landoFilesToAnalyse.map(landoAnalyzer.parseDocument)
    val sysMLDocuments = sysmlFilesToAnalyse.map(sysmlAnalyzer.parseDocument)
    val cryptolDocuments = cryptolFilesToAnalyse.map(cryptolAnalyzer.parseDocument)
    val bsvDocuments = bsvFilesToAnalyse.map(bsvAnalyzer.parseDocument)
    val svDocuments = svFilesToAnalyse.map(svAnalyzer.parseDocument)

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
    //val enrichedBSVDocuments = bsvDocuments.map(doc => bsvReferencer.addRefinementRelations(doc, Array.empty[DocumentInfo], Array.empty[DocumentInfo]))
    //val enrichedSVDocuments = svDocuments.map(doc => svReferencer.addRefinementRelations(doc, Array.empty[DocumentInfo], Array.empty[DocumentInfo]))

    enrichedLandoDocuments.foreach(doc => addReferences(doc, enrichedLandoDocuments.flatMap(_.getAllReferences).toSet))
    enrichedSysMLDocuments.foreach(doc => addReferences(doc, enrichedSysMLDocuments.flatMap(_.getAllReferences).toSet))
    enrichedCryptolDocuments.foreach(doc => addReferences(doc, enrichedCryptolDocuments.flatMap(_.getAllReferences).toSet))
    //enrichedBSVDocuments.foreach(doc => addReferences(doc, enrichedBSVDocuments.flatMap(_.getAllReferences).toSet))
    //enrichedSVDocuments.foreach(doc => addReferences(doc, enrichedSVDocuments.flatMap(_.getAllReferences).toSet))

    val result = enrichedLandoDocuments ++ enrichedSysMLDocuments ++ enrichedCryptolDocuments ++ bsvDocuments ++ svDocuments

    result.forall(file => {
      assert(filesToAnalyze.exists(_.equals(file.filePath)), s"File ${file.documentName} was not found in the list of files to analyze")
      true
    })
    result
  } ensuring ((res: Array[DocumentInfo]) => res.length == filesToAnalyze.length, "Not all files were analyzed")

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

  def addExplicitRefinements(report: ReportReference, refinements: Set[RefinementModel]): ReportReference = {
    val allReferences: Map[String, Array[DocReference]] = report.getAllReferences.groupBy(ref => ref.documentName)
    //Both ends of the refinement must be in the report source code
    val allValidRefinements = refinements.filter(
      refinement => allReferences.keySet.contains(refinement.srcRef.file) &&
        allReferences.keySet.contains(refinement.trgRef.file) &&
        allReferences(refinement.srcRef.file).exists(_.getName.equalsIgnoreCase(refinement.srcRef.ref)) &&
        allReferences(refinement.trgRef.file).exists(_.getName.equalsIgnoreCase(refinement.trgRef.ref)))

    allValidRefinements.foldLeft(report)((accReport, refinement) => {
      val srcRef = allReferences(refinement.srcRef.file).filter(_.getName.equalsIgnoreCase(refinement.srcRef.ref)).head
      val trgRef = allReferences(refinement.trgRef.file).filter(_.getName.equalsIgnoreCase(refinement.trgRef.ref)).head
      val updatedSrcRef = srcRef.addRefinement(trgRef)
      val updatedTargetRef = trgRef.addAbstraction(srcRef)
      accReport.updateDocumentByName(updatedSrcRef.documentName, updatedSrcRef)
        .updateDocumentByName(updatedTargetRef.documentName, updatedTargetRef)
    })
  }
}
