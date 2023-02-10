package Analyzers

import ConfigParser.{FileDocRef, RefinementModel}
import DocumentEnrichers.{DocumentEnricher, _}
import Formatter.LatexFormatter
import Referencer._
import Report.ReportTypes.{Documents, ReportReference}
import Specs.FileSpecs
import Types.DocReference.DocReference
import Types.DocumentInfos.{DocumentInfo, _}
import Utils.{FileUtil, Matcher}

import java.nio.file.Paths
import scala.reflect.ClassTag

object DocumentAnalyzer {
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

    report.moveFiles(report.folder)
  } //ensuring ((report: ReportReference) => FileSpecs.allFilesAnalyzed(filesToAnalyze, report))

  private def enrichFiles(filesToAnalyze: Set[String], latexDocumentData: LatexDocumentData, explicitReferences: Set[RefinementModel] = Set.empty[RefinementModel]): ReportReference = {
    require(filesToAnalyze.nonEmpty, "No files to analyze")
    require(FileSpecs.fileChecks(filesToAnalyze, supportedTypes), "Not all files exist or are of one of the supported types")
    val formatter = latexDocumentData.latexFormatter
    val enrichedDocuments = enrichDocuments(filesToAnalyze.toArray, formatter)

    val report = ReportReference(
      latexDocumentData.title,
      latexDocumentData.author,
      latexDocumentData.folder,
      enrichedDocuments,
      latexDocumentData.layout
    )
    val updatedReport = report // addExplicitRefinements(report, explicitReferences)

    enrichReport(updatedReport, formatter)
  } ensuring ((res: ReportReference) => FileSpecs.allFilesAnalyzed(filesToAnalyze, res))

  private def enricherPerDocumentType[T <: DocumentInfo[T], D <: DocumentEnricher[T]](formatter: LatexFormatter, doc: T): D = {
    doc.documentType match {
      case Types.DocumentType.Lando => new LandoDocumentEnricher(formatter).asInstanceOf[D]
      case Types.DocumentType.Lobot => new LobotDocumentEnricher(formatter).asInstanceOf[D]
      case Types.DocumentType.SysML => new SysMLDocumentEnricher(formatter).asInstanceOf[D]
      case Types.DocumentType.Cryptol => new CryptolDocumentEnricher(formatter).asInstanceOf[D]
      case Types.DocumentType.Saw => new SawDocumentEnricher(formatter).asInstanceOf[D]
      case Types.DocumentType.BSV => new BSVDocumentEnricher(formatter).asInstanceOf[D]
      case Types.DocumentType.SV => new SVDocumentEnricher(formatter).asInstanceOf[D]
      case Types.DocumentType.C => new ACSLDocumentEnricher(formatter).asInstanceOf[D]
    }
  }

  private def enrichReport(reportReference: ReportReference, formatter: LatexFormatter): ReportReference = {
    val lando = reportReference.documents.landoDocuments.map(doc => doc.decorate(enricherPerDocumentType[LandoDocumentInfo, LandoDocumentEnricher](formatter, doc)))
    val lobot = reportReference.documents.lobotDocuments.map(doc => doc.decorate(enricherPerDocumentType[LobotDocumentInfo, LobotDocumentEnricher](formatter, doc)))
    val sysML = reportReference.documents.sysmlDocuments.map(doc => doc.decorate(enricherPerDocumentType[SysMLDocumentInfo, SysMLDocumentEnricher](formatter, doc)))
    val cryptol = reportReference.documents.cryptolDocuments.map(doc => doc.decorate(enricherPerDocumentType[CryptolDocumentInfo, CryptolDocumentEnricher](formatter, doc)))
    val saw = reportReference.documents.sawDocuments.map(doc => doc.decorate(enricherPerDocumentType[SawDocumentInfo, SawDocumentEnricher](formatter, doc)))
    val bsv = reportReference.documents.bsvDocuments.map(doc => doc.decorate(enricherPerDocumentType[BSVDocumentInfo, BSVDocumentEnricher](formatter, doc)))
    val sv = reportReference.documents.svDocuments.map(doc => doc.decorate(enricherPerDocumentType[SVDocumentInfo, SVDocumentEnricher](formatter, doc)))
    val c = reportReference.documents.cDocuments.map(doc => doc.decorate(enricherPerDocumentType[CDocumentInfo, ACSLDocumentEnricher](formatter, doc)))

    val updatedDocuments = Documents(lando, lobot, sysML, cryptol, saw, bsv, sv, c)
    reportReference.copy(documents = updatedDocuments)
  }


  private def parseDocumentsOfType[T <: DocumentInfo[T], D <: DocumentEnricher[T]](
                                                                                    files: Array[String],
                                                                                    documentType: Types.DocumentType.Value,
                                                                                    parser: D)(implicit classTag: ClassTag[T]): Array[T] = {
    val filesOfType = files.filter(file => FileUtil.getFileType(file).compareToIgnoreCase(documentType.toString) == 0)
    filesOfType.map(file => parser.parseDocument(file))
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

    val enrichedBSVDocuments = bsvDocuments.map(doc => BlueSpecReferencer.addRefinementRelations(doc, Array.empty[CryptolDocumentInfo], Array.empty[BSVDocumentInfo]))
    val enrichedSVDocuments = svDocuments.map(doc => SystemVerilogReferencer.addRefinementRelations(doc, Array.empty[CryptolDocumentInfo], Array.empty[SVDocumentInfo]))
    val enrichedCryptolDocuments = cryptolDocuments.map(doc => CryptolReferencer.addRefinementRelations(doc, sysMLDocuments, enrichedBSVDocuments))
    val enrichedSysMLDocuments = sysMLDocuments.map(doc => SysMLReferencer.addRefinementRelations(doc, landoDocuments, enrichedCryptolDocuments))
    val enrichedLandoDocuments = landoDocuments.map(doc => LandoReferencer.addRefinementRelations(doc, Array.empty[LandoDocumentInfo], enrichedSysMLDocuments))

    /*
    def addInternalReferences[T <: DocumentInfo[T]](docs: Array[T]): Unit = {
      docs.foreach(doc => addReferences(doc, docs.flatMap(_.getAllReferences).toSet))
    }

    addInternalReferences(enrichedLandoDocuments)
    addInternalReferences(enrichedSysMLDocuments)
    addInternalReferences(enrichedCryptolDocuments)
    addInternalReferences(enrichedBSVDocuments)
    addInternalReferences(enrichedSVDocuments)

     */

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
}


trait DocumentReferencer {
  /*
  def addReferences(documents: Documents): Documents = {
    //addImplicitRefinements(documents.allDocuments)
    //addExplicitRefinements(documents)
  }
   */

  private def addImplicitReferences[T <: DocumentInfo[T]](document: T, references: Set[DocReference]): Unit = {
    val referencesToUpdate: Set[DocReference] = document.getAllReferences.filter(ref => ref.isReferencingAnything)
    referencesToUpdate.foreach(reference => {
      val potentialReferences: Map[String, DocReference] = {
        references.flatMap(r => {
          reference.getStringReferences.get
            .map(ref => Matcher.getReferenceName(ref.name, r.getReferenceName))
            .filter(_.isDefined)
            .map(_.get)
            .map(ref => (ref, r))
        }).toMap
      }
      potentialReferences.foreach(ref => reference.addReference(ref))
    })
  }

  private def addImplicitRefinements[T <: DocumentInfo[T]](docs: Array[T]): Array[T] = {
    require(docs.nonEmpty, "No documents to analyze")
    docs.foreach(doc => addImplicitReferences(doc, docs.flatMap(_.getAllReferences).toSet))
    docs
  } //ensuring((res: Array[T]) => FileSpecs.allFilesAnalyzed(filesToAnalyze, res), "Not all files were analyzed")


  private def addExplicitRefinements(report: Documents, refinements: Set[RefinementModel]): Documents = {
    val allReferences: Map[String, Array[DocReference]] = report.getAllReferences.groupBy(ref => ref.documentName)

    def getReference(ref: RefinementModel, ext: RefinementModel => FileDocRef): Array[DocReference] = {
      allReferences(ext(ref).file).filter(_.getName.equalsIgnoreCase(ext(ref).ref))
    }

    //Both ends of the refinement must be in the report source code and must be valid references otherwise the refinement is ignored
    val allValidRefinements = refinements.filter(
      refinement => allReferences.keySet.contains(refinement.srcRef.file) &&
        allReferences.keySet.contains(refinement.trgRef.file) &&
        getReference(refinement, _.srcRef).nonEmpty &&
        getReference(refinement, _.trgRef).nonEmpty)

    allValidRefinements.foldLeft(report)((accReport, refinement) => {
      val srcRef = getReference(refinement, _.srcRef).head
      val trgRef = getReference(refinement, _.trgRef).head
      val updatedSrcRef = srcRef.addRefinement(trgRef)
      val updatedTargetRef = trgRef.addAbstraction(srcRef)
      accReport.updateDocumentByName(updatedSrcRef.documentName, updatedSrcRef)
        .updateDocumentByName(updatedTargetRef.documentName, updatedTargetRef)
    })
  }
}