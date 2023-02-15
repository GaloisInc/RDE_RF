package Analyzers

import ConfigParser.RefinementModel
import DocumentEnrichers._
import Formatter.LatexFormatter
import Report.ReportTypes.{Documents, ReportReference}
import Specs.FileSpecs
import Types.DocumentInfos._
import Utils.FileUtil

import scala.reflect.ClassTag

object DocumentAnalyzer {
  val supportedTypes: Set[String] = AnalyzerSettings.supportedDocumentTypesString

  def generateReport(filesToAnalyze: Set[String],
                     latexDocumentData: LatexDocumentData,
                     explicitRefinements: Set[RefinementModel],
                     sortFiles: Boolean = true): ReportReference = {
    require(filesToAnalyze.nonEmpty, "No files to analyze")
    require(FileSpecs.fileChecks(filesToAnalyze, supportedTypes), "Not all files exist or are of one of the supported types")
    val report = enrichFiles(filesToAnalyze, latexDocumentData, explicitRefinements)
    if (sortFiles) report.moveFiles(report.folder)
    else report
  } ensuring ((report: ReportReference) => FileSpecs.allFilesAnalyzed(filesToAnalyze, report))

  private def enrichFiles(filesToAnalyze: Set[String], latexDocumentData: LatexDocumentData, explicitReferences: Set[RefinementModel] = Set.empty[RefinementModel]): ReportReference = {
    require(filesToAnalyze.nonEmpty, "No files to analyze")
    require(FileSpecs.fileChecks(filesToAnalyze, supportedTypes), "Not all files exist or are of one of the supported types")
    val formatter = latexDocumentData.latexFormatter
    val documents = parseDocuments(filesToAnalyze.toArray, formatter)
    val documentsWithReference = DocumentReferencer.addReferences(documents, explicitReferences)
    val decoratedSourceFiles = decorateSourceFiles(documentsWithReference, formatter)

    ReportReference(
      latexDocumentData.title,
      latexDocumentData.author,
      latexDocumentData.folder,
      decoratedSourceFiles,
      latexDocumentData.layout
    )
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
      case Types.DocumentType.Fret => new FRETDocumentEnricher(formatter).asInstanceOf[D]
    }
  }

  private def decorateSourceFiles(documents: Documents, formatter: LatexFormatter): Documents = {
    val lando = documents.landoDocuments.map(doc => doc.decorate(enricherPerDocumentType[LandoDocumentInfo, LandoDocumentEnricher](formatter, doc)))
    val lobot = documents.lobotDocuments.map(doc => doc.decorate(enricherPerDocumentType[LobotDocumentInfo, LobotDocumentEnricher](formatter, doc)))
    val sysML = documents.sysmlDocuments.map(doc => doc.decorate(enricherPerDocumentType[SysMLDocumentInfo, SysMLDocumentEnricher](formatter, doc)))
    val cryptol = documents.cryptolDocuments.map(doc => doc.decorate(enricherPerDocumentType[CryptolDocumentInfo, CryptolDocumentEnricher](formatter, doc)))
    val fret = documents.fretDocuments.map(doc => doc.decorate(enricherPerDocumentType[FretDocument, FRETDocumentEnricher](formatter, doc)))
    val saw = documents.sawDocuments.map(doc => doc.decorate(enricherPerDocumentType[SawDocumentInfo, SawDocumentEnricher](formatter, doc)))
    val bsv = documents.bsvDocuments.map(doc => doc.decorate(enricherPerDocumentType[BSVDocumentInfo, BSVDocumentEnricher](formatter, doc)))
    val sv = documents.svDocuments.map(doc => doc.decorate(enricherPerDocumentType[SVDocumentInfo, SVDocumentEnricher](formatter, doc)))
    val c = documents.cDocuments.map(doc => doc.decorate(enricherPerDocumentType[CDocumentInfo, ACSLDocumentEnricher](formatter, doc)))
    Documents(lando, lobot, sysML, cryptol, saw, bsv, sv, c, fret)
  } // TODO: add postcondition


  private def parseDocuments(filesToAnalyze: Array[String], formatter: LatexFormatter): Documents = {
    require(filesToAnalyze.nonEmpty, "No files to analyze")
    require(FileSpecs.fileChecks(filesToAnalyze.toSet, supportedTypes), "Not all files exist or are of one of the supported types")

    // Parse files of a specific type into a specific document type
    def parseDocumentsOfType[T <: DocumentInfo[T], D <: DocumentEnricher[T]](
                                                                              files: Array[String],
                                                                              documentType: Types.DocumentType.documentType,
                                                                              parser: D)(implicit classTag: ClassTag[T]): Array[T] = {
      val filesOfType = files.filter(file => FileUtil.getDocumentType(file) == documentType)
      filesOfType.map(file => parser.parseDocument(file))
    }

    // Parse all files
    val landoDocuments: Array[LandoDocumentInfo] = parseDocumentsOfType[LandoDocumentInfo, LandoDocumentEnricher](filesToAnalyze, Types.DocumentType.Lando, new LandoDocumentEnricher(formatter))
    val lobotDocuments: Array[LobotDocumentInfo] = parseDocumentsOfType[LobotDocumentInfo, LobotDocumentEnricher](filesToAnalyze, Types.DocumentType.Lobot, new LobotDocumentEnricher(formatter))
    val sysMLDocuments: Array[SysMLDocumentInfo] = parseDocumentsOfType[SysMLDocumentInfo, SysMLDocumentEnricher](filesToAnalyze, Types.DocumentType.SysML, new SysMLDocumentEnricher(formatter))
    val cryptolDocuments: Array[CryptolDocumentInfo] = parseDocumentsOfType[CryptolDocumentInfo, CryptolDocumentEnricher](filesToAnalyze, Types.DocumentType.Cryptol, new CryptolDocumentEnricher(formatter))
    val sawDocuments: Array[SawDocumentInfo] = parseDocumentsOfType[SawDocumentInfo, SawDocumentEnricher](filesToAnalyze, Types.DocumentType.Saw, new SawDocumentEnricher(formatter))
    val bsvDocuments: Array[BSVDocumentInfo] = parseDocumentsOfType[BSVDocumentInfo, BSVDocumentEnricher](filesToAnalyze, Types.DocumentType.BSV, new BSVDocumentEnricher(formatter))
    val svDocuments: Array[SVDocumentInfo] = parseDocumentsOfType[SVDocumentInfo, SVDocumentEnricher](filesToAnalyze, Types.DocumentType.SV, new SVDocumentEnricher(formatter))
    val cDocuments: Array[CDocumentInfo] = parseDocumentsOfType[CDocumentInfo, ACSLDocumentEnricher](filesToAnalyze, Types.DocumentType.C, new ACSLDocumentEnricher(formatter))
    val fretDocuments: Array[FretDocument] = parseDocumentsOfType[FretDocument, FRETDocumentEnricher](filesToAnalyze, Types.DocumentType.Fret, new FRETDocumentEnricher(formatter))

    Documents(landoDocuments, lobotDocuments, sysMLDocuments, cryptolDocuments, sawDocuments, bsvDocuments, svDocuments, cDocuments, fretDocuments)
  } ensuring((res: Documents) => res.numberOfDocuments == filesToAnalyze.length, "Not all files were parsed")
}