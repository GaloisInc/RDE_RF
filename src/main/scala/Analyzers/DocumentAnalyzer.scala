package Analyzers

import ConfigParser.RefinementModel
import DocumentEnrichers._
import Formatter.LatexFormatter
import Referencer._
import Report.ReportTypes.ReportReference
import Specs.FileSpecs
import Specs.FileSpecs.{allFilesExist, allFilesOfCorrectType}
import Types.DocReference.DocReference
import Types.DocumentInfos._
import Utils.{FileUtil, Matcher}

import java.nio.file.Paths

object DocumentAnalyzer {
  //Referencers
  private val landoReferencer = new LandoReferencer()
  private val sysMLReferencer = new SysMLReferencer()
  private val cryptolReferencer = new CryptolReferencer()
  private val bsvReferencer = new BlueSpecReferencer()
  private val svReferencer = new SystemVerilogReferencer()

  val supportedTypes = Set[String]("lando", "sysml", "bsv", "sv", "cry")


  def generateReport(filesToAnalyze: Set[String],
                     latexDocumentData: LatexDocumentData,
                     explicitRefinements: Set[RefinementModel],
                     sortFiles: Boolean = true): ReportReference = {
    require(filesToAnalyze.nonEmpty, "No files to analyze")
    require(FileSpecs.fileChecks(filesToAnalyze, supportedTypes), "Not all files exist or are of one of the supported types")


    if (sortFiles) enrichAndMoveFiles(filesToAnalyze, latexDocumentData, explicitRefinements)
    else enrichFiles(filesToAnalyze, latexDocumentData, explicitRefinements)
  }

  def enrichAndMoveFiles(filesToAnalyze: Set[String], latexDocumentData: LatexDocumentData, explicitRefinements: Set[RefinementModel]): ReportReference = {
    require(filesToAnalyze.nonEmpty, "No files to analyze")
    require(FileSpecs.fileChecks(filesToAnalyze, supportedTypes), "Not all files exist or are of one of the supported types")
    val report = enrichFiles(filesToAnalyze, latexDocumentData, explicitRefinements)
    moveFilesInReport(report)
  } ensuring ((report: ReportReference) => FileSpecs.allFilesAnalyzed(filesToAnalyze, report))

  def moveFilesInReport(reportReference: ReportReference): ReportReference = {
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

  def enrichFiles(filesToAnalyze: Set[String], latexDocumentData: LatexDocumentData, explicitReferences: Set[RefinementModel] = Set.empty[RefinementModel]): ReportReference = {
    require(filesToAnalyze.nonEmpty, "No files to analyze")
    require(FileSpecs.fileChecks(filesToAnalyze, supportedTypes), "Not all files exist or are of one of the supported types")

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
  } ensuring ((res: ReportReference) => FileSpecs.allFilesAnalyzed(filesToAnalyze, res))


  def enrichReport(reportReference: ReportReference, formatter: LatexFormatter): ReportReference = {
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

  def enrichDocuments(filesToAnalyze: Set[String], formatter: LatexFormatter): Array[DocumentInfo] = {
    require(filesToAnalyze.nonEmpty, "No files to analyze")
    require(FileSpecs.fileChecks(filesToAnalyze, supportedTypes), "Not all files exist or are of one of the supported types")

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

    //assert(landoDocuments.intersect(sysMLDocuments) == Set.empty)
    //assert(landoDocuments.intersect(cryptolDocuments) == Set.empty)
    //assert(sysMLDocuments.intersect(cryptolDocuments) == Set.empty)
    //Ensuring Unique references/labels
    assert(landoDocuments.flatMap(_.getAllReferences.map(_.getLabelText)).size == landoDocuments.flatMap(_.getAllReferences.map(_.getLabelText)).size)
    assert(cryptolDocuments.flatMap(_.getAllReferences.map(_.getLabelText)).size == cryptolDocuments.flatMap(_.getAllReferences.map(_.getLabelText)).size)
    assert(sysMLDocuments.flatMap(_.getAllReferences.map(_.getLabelText)).size == sysMLDocuments.flatMap(_.getAllReferences.map(_.getLabelText)).size)
    assert(svDocuments.flatMap(_.getAllReferences.map(_.getLabelText)).size == svDocuments.flatMap(_.getAllReferences.map(_.getLabelText)).size)
    assert(bsvDocuments.flatMap(_.getAllReferences.map(_.getLabelText)).size == bsvDocuments.flatMap(_.getAllReferences.map(_.getLabelText)).size)

    val enrichedCryptolDocuments = cryptolDocuments.map(doc => cryptolReferencer.addRefinementRelations(doc, sysMLDocuments.map(_.asInstanceOf[DocumentInfo]).toArray, Array.empty[DocumentInfo]))
    val enrichedSysMLDocuments = sysMLDocuments.map(doc => sysMLReferencer.addRefinementRelations(doc, landoDocuments.map(_.asInstanceOf[DocumentInfo]).toArray, enrichedCryptolDocuments.toArray))
    val enrichedLandoDocuments = landoDocuments.map(doc => landoReferencer.addRefinementRelations(doc, Array.empty[DocumentInfo], enrichedSysMLDocuments.toArray))
    val enrichedBSVDocuments = bsvDocuments.map(doc => bsvReferencer.addRefinementRelations(doc, Array.empty[DocumentInfo], Array.empty[DocumentInfo]))
    val enrichedSVDocuments = svDocuments.map(doc => svReferencer.addRefinementRelations(doc, Array.empty[DocumentInfo], Array.empty[DocumentInfo]))

    enrichedLandoDocuments.foreach(doc => addReferences(doc, enrichedLandoDocuments.flatMap(_.getAllReferences).toSet))
    enrichedSysMLDocuments.foreach(doc => addReferences(doc, enrichedSysMLDocuments.flatMap(_.getAllReferences).toSet))
    enrichedCryptolDocuments.foreach(doc => addReferences(doc, enrichedCryptolDocuments.flatMap(_.getAllReferences).toSet))
    enrichedBSVDocuments.foreach(doc => addReferences(doc, enrichedBSVDocuments.flatMap(_.getAllReferences).toSet))
    enrichedSVDocuments.foreach(doc => addReferences(doc, enrichedSVDocuments.flatMap(_.getAllReferences).toSet))

    val result = enrichedLandoDocuments ++ enrichedSysMLDocuments ++ enrichedCryptolDocuments ++ enrichedSVDocuments ++ enrichedBSVDocuments

    result.toArray
  } ensuring((res: Array[DocumentInfo]) => FileSpecs.allFilesAnalyzed(filesToAnalyze,res), "Not all files were analyzed")

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
