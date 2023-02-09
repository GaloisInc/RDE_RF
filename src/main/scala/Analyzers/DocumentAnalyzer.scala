package Analyzers

import ConfigParser.RefinementModel
import DocumentEnrichers._
import Formatter.LatexFormatter
import Referencer._
import Report.ReportTypes.{Documents, ReportReference}
import Specs.FileSpecs
import Types.DocReference.DocReference
import Types.DocumentInfos._
import Utils.{FileUtil, Matcher}
import java.nio.file.Paths
import scala.reflect.ClassTag

object DocumentAnalyzer {
  //Referencers
  private val landoReferencer = new LandoReferencer()
  private val sysMLReferencer = new SysMLReferencer()
  private val cryptolReferencer = new CryptolReferencer()
  private val bsvReferencer = new BlueSpecReferencer()
  private val svReferencer = new SystemVerilogReferencer()

  val supportedTypes: Set[String] = AnalyzerSettings.supportedDocumentTypesString

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
    val report: ReportReference = enrichFiles(filesToAnalyze, latexDocumentData, explicitRefinements)
    val targetFolder = report.folder
    report.copy(
      landoDocuments = moveFiles[LandoDocumentInfo](report.landoDocuments, targetFolder, Types.DocumentType.Lando),
      lobotDocuments = moveFiles[LobotDocumentInfo](report.lobotDocuments, targetFolder, Types.DocumentType.Lobot),
      sysmlDocuments = moveFiles[SysMLDocumentInfo](report.sysmlDocuments, targetFolder, Types.DocumentType.SysML),
      cryptolDocuments = moveFiles[CryptolDocumentInfo](report.cryptolDocuments, targetFolder, Types.DocumentType.Cryptol),
      sawDocuments = moveFiles[SawDocumentInfo](report.sawDocuments, targetFolder, Types.DocumentType.Saw),
      bsvDocuments = moveFiles[BSVDocumentInfo](report.bsvDocuments, targetFolder, Types.DocumentType.BSV),
      svDocuments = moveFiles[SVDocumentInfo](report.svDocuments, targetFolder, Types.DocumentType.SV),
      cDocuments = moveFiles[CDocumentInfo](report.cDocuments, targetFolder, Types.DocumentType.C)
    )
  } //ensuring ((report: ReportReference) => FileSpecs.allFilesAnalyzed(filesToAnalyze, report))

  private def moveFiles[A <: DocumentInfo[A]](filesToMove: Array[A], destination: String, documentType: Types.DocumentType.Value)
                                             (implicit classTag: ClassTag[A]) = {
    val destinationPath = Paths.get(destination, documentType.toString).toString
    filesToMove.map(file => {
      val filePath = FileUtil.moveRenameFile(file.filePath, destinationPath)
      file.updateFilePath(filePath)
    })
  }

  private def enrichFiles[T <: DocumentInfo[T]](filesToAnalyze: Set[String], latexDocumentData: LatexDocumentData, explicitReferences: Set[RefinementModel] = Set.empty[RefinementModel]): ReportReference = {
    require(filesToAnalyze.nonEmpty, "No files to analyze")
    require(FileSpecs.fileChecks(filesToAnalyze, supportedTypes), "Not all files exist or are of one of the supported types")
    val formatter = latexDocumentData.latexFormatter
    val enrichedDocuments = enrichDocuments(filesToAnalyze.toArray, formatter)

    val report = ReportReference(
      latexDocumentData.title,
      latexDocumentData.author,
      latexDocumentData.folder,
      enrichedDocuments.landoDocuments,
      enrichedDocuments.lobotDocuments,
      enrichedDocuments.sysmlDocuments,
      enrichedDocuments.cryptolDocuments,
      enrichedDocuments.sawDocuments,
      enrichedDocuments.bsvDocuments,
      enrichedDocuments.svDocuments,
      enrichedDocuments.cDocuments,
      latexDocumentData.layout
    )
    val updatedReport = addExplicitRefinements(report, explicitReferences)

    enrichReport(updatedReport, formatter)
  } ensuring ((res: ReportReference) => FileSpecs.allFilesAnalyzed(filesToAnalyze, res))


  private def enrichReport(reportReference: ReportReference, formatter: LatexFormatter): ReportReference = {
    def decorateFiles[T <: DocumentInfo[T]](filesToDecorate: Array[T], enricher: DocumentEnricher[T])(implicit ct: ClassTag[T]): Array[T] = {
      filesToDecorate.map(file => {
        file.updateFilePath(enricher.decorateFile(file))
      })
    }

    reportReference.copy(
      landoDocuments = decorateFiles(reportReference.landoDocuments, new LandoDocumentEnricher(formatter)),
      lobotDocuments = decorateFiles(reportReference.lobotDocuments, new LobotDocumentEnricher(formatter)),
      sysmlDocuments = decorateFiles(reportReference.sysmlDocuments, new SysMLDocumentEnricher(formatter)),
      cryptolDocuments = decorateFiles(reportReference.cryptolDocuments, new CryptolDocumentEnricher(formatter)),
      sawDocuments = decorateFiles(reportReference.sawDocuments, new SawDocumentEnricher(formatter)),
      bsvDocuments = decorateFiles(reportReference.bsvDocuments, new BSVDocumentEnricher(formatter)),
      svDocuments = decorateFiles(reportReference.svDocuments, new SVDocumentEnricher(formatter)),
      cDocuments = decorateFiles(reportReference.cDocuments, new ACSLDocumentEnricher(formatter))
    )
  }


  private def parseDocumentsOfType[T <: DocumentInfo[T], D <: DocumentEnricher[T]](
                                                                                    files: Array[String],
                                                                                    documentType: Types.DocumentType.Value,
                                                                                    documentParser: D)(implicit classTag: ClassTag[T]): Array[T] = {
    val filesOfType = files.filter(file => FileUtil.getFileType(file).compareToIgnoreCase(documentType.toString) == 0)
    filesOfType.map(file => documentParser.parseDocument(file))
  }

  private def enrichDocuments(filesToAnalyze: Array[String], formatter: LatexFormatter): Documents = {
    require(filesToAnalyze.nonEmpty, "No files to analyze")
    require(FileSpecs.fileChecks(filesToAnalyze.toSet, supportedTypes), "Not all files exist or are of one of the supported types")
    // Parse all files
    val landoDocuments: Array[LandoDocumentInfo] = parseDocumentsOfType[LandoDocumentInfo, LandoDocumentEnricher](filesToAnalyze, Types.DocumentType.Lando, new LandoDocumentEnricher(formatter))
    val lobotDocuments: Array[LobotDocumentInfo] = parseDocumentsOfType[LobotDocumentInfo, LobotDocumentEnricher](filesToAnalyze, Types.DocumentType.Lobot, new LobotDocumentEnricher(formatter))
    val sysMLDocuments: Array[SysMLDocumentInfo] = parseDocumentsOfType[SysMLDocumentInfo, SysMLDocumentEnricher](filesToAnalyze, Types.DocumentType.SysML, new SysMLDocumentEnricher(formatter))
    val cryptolDocuments: Array[CryptolDocumentInfo] = parseDocumentsOfType[CryptolDocumentInfo, CryptolDocumentEnricher](filesToAnalyze, Types.DocumentType.Cryptol, new CryptolDocumentEnricher(formatter))
    val sawDocuments: Array[SawDocumentInfo] = parseDocumentsOfType[SawDocumentInfo, SawDocumentEnricher](filesToAnalyze, Types.DocumentType.Saw, new SawDocumentEnricher(formatter))
    val bsvDocuments: Array[BSVDocumentInfo] = parseDocumentsOfType[BSVDocumentInfo, BSVDocumentEnricher](filesToAnalyze, Types.DocumentType.BSV, new BSVDocumentEnricher(formatter))
    val svDocuments: Array[SVDocumentInfo] = parseDocumentsOfType[SVDocumentInfo, SVDocumentEnricher](filesToAnalyze, Types.DocumentType.SV, new SVDocumentEnricher(formatter))
    val cDocuments: Array[CDocumentInfo] = parseDocumentsOfType[CDocumentInfo, ACSLDocumentEnricher](filesToAnalyze, Types.DocumentType.C, new ACSLDocumentEnricher(formatter))

    val enrichedCryptolDocuments = cryptolDocuments.map(doc => cryptolReferencer.addRefinementRelations(doc, sysMLDocuments, Array.empty[CryptolDocumentInfo]))
    val enrichedSysMLDocuments = sysMLDocuments.map(doc => sysMLReferencer.addRefinementRelations(doc, landoDocuments, enrichedCryptolDocuments))
    val enrichedLandoDocuments = landoDocuments.map(doc => landoReferencer.addRefinementRelations(doc, Array.empty[LandoDocumentInfo], enrichedSysMLDocuments))
    val enrichedBSVDocuments = bsvDocuments.map(doc => bsvReferencer.addRefinementRelations(doc, Array.empty[CryptolDocumentInfo], Array.empty[BSVDocumentInfo]))
    val enrichedSVDocuments = svDocuments.map(doc => svReferencer.addRefinementRelations(doc, Array.empty[CryptolDocumentInfo], Array.empty[SVDocumentInfo]))

    def addInternalReferences[T <: DocumentInfo[T]](docs: Array[T]): Unit = {
      docs.foreach(doc => addReferences(doc, docs.flatMap(_.getAllReferences).toSet))
    }

    addInternalReferences(enrichedLandoDocuments)
    addInternalReferences(enrichedSysMLDocuments)
    addInternalReferences(enrichedCryptolDocuments)
    addInternalReferences(enrichedBSVDocuments)
    addInternalReferences(enrichedSVDocuments)

    Documents(
      enrichedLandoDocuments,
      lobotDocuments,
      enrichedSysMLDocuments,
      enrichedCryptolDocuments,
      sawDocuments,
      enrichedBSVDocuments,
      enrichedSVDocuments,
      cDocuments
    )
  } //ensuring((res: Array[T]) => FileSpecs.allFilesAnalyzed(filesToAnalyze, res), "Not all files were analyzed")

  def addReferences[T <: DocumentInfo[T]](document: T, references: Set[DocReference]): Unit = {
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

  private def addExplicitRefinements(report: ReportReference, refinements: Set[RefinementModel]): ReportReference = {
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
      //accReport.updateDocumentByName(updatedSrcRef.documentName, updatedSrcRef)
      //  .updateDocumentByName(updatedTargetRef.documentName, updatedTargetRef)
      accReport
    })
  }
}
