package Analyzers

import DocumentEnrichers.*
import Formatter.{InlineFormatter, LatexFormatter}
import Referencer.*
import Report.PaperLayout
import Report.ReportTypes.ReportReference
import Types.DocReference.DocReference
import Types.DocumentInfos.{CryptolDocumentInfo, DocumentInfo, LandoDocumentInfo, SysMLDocumentInfo}
import Types.ReferenceType
import Utils.*

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
  private val landoReferencer = LandoReferencer()
  private val sysMLReferencer = SysMLReferencer()
  private val cryptolReferencer = CryptolReferencer()


  def generateReport(filesToAnalyze: Array[String],
                     latexDocumentData: LatexDocumentData,
                     sortFiles: Boolean = true): ReportReference = {
    require(filesToAnalyze.nonEmpty, "No files to analyze")

      if sortFiles then
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
    val landoAnalyzer = LandoDocumentEnricher(formatter)
    val sysMLAnalyzer = SysMLDocumentEnricher(formatter)
    val cryptolAnalyzer = CryptolDocumentEnricher(formatter)

    val enrichedDocuments = enrichDocuments(filesToAnalyze, formatter)

    val enrichedLandoDocuments = FileUtil.getLandoDocuments(enrichedDocuments)
    val enrichedSysMLDocuments = FileUtil.getSysMLDocuments(enrichedDocuments)
    val enrichedCryptolDocuments = FileUtil.getCryptolDocuments(enrichedDocuments)

    assert(enrichedCryptolDocuments.intersect(enrichedSysMLDocuments).isEmpty)
    assert(enrichedLandoDocuments.intersect(enrichedSysMLDocuments).isEmpty)
    assert(enrichedCryptolDocuments.intersect(enrichedLandoDocuments).isEmpty)

    val decoratedLando = enrichedLandoDocuments.map(doc => {
      val filePath = landoAnalyzer.enrichFile(doc)
      doc.asInstanceOf[LandoDocumentInfo].copy(filePath = filePath)
    })
    val decoratedSysML = enrichedSysMLDocuments.map(doc => {
      val filePath = sysMLAnalyzer.enrichFile(doc)
      doc.asInstanceOf[SysMLDocumentInfo].copy(filePath = filePath)
    })
    val decoratedCryptol = enrichedCryptolDocuments.map(doc => {
      val filePath = cryptolAnalyzer.enrichFile(doc)
      doc.asInstanceOf[CryptolDocumentInfo].copy(filePath = filePath)
    })

    ReportReference(latexDocumentData.title, latexDocumentData.folder, decoratedLando, decoratedSysML, decoratedCryptol, latexDocumentData.layout)
  } ensuring ((res: ReportReference) => res.cryptolDocuments.length + res.sysmlDocuments.length + res.landoDocuments.length == filesToAnalyze.length)


  def nonRefinementLando(filesToAnalyze: Array[String]): Array[DocReference] = {
    require(filesToAnalyze.nonEmpty)

    val enrichedDocuments = enrichDocuments(filesToAnalyze, InlineFormatter())
    val enrichedLandoDocuments = FileUtil.getLandoDocuments(enrichedDocuments)

    val nonSpecializedConstructs = enrichedLandoDocuments.flatMap(doc => doc.getAllReferences.filter(ref => ref.getAbstractions.isEmpty))
    nonSpecializedConstructs
  }

  def nonSpecializedLandoConstructs(documents: Array[DocumentInfo]): Array[DocReference] = {
    require(documents.nonEmpty)
    val enrichedLandoDocuments = FileUtil.getLandoDocuments(documents)
    val nonSpecializedConstructs = enrichedLandoDocuments.flatMap(doc => doc.getAllReferences.filter(ref => ref.getAbstractions.isEmpty))
    nonSpecializedConstructs
  } ensuring ((nonSpecialized: Array[DocReference]) => {
    val allReferences = documents.flatMap(doc => doc.getAllReferences).toSet
    nonSpecialized.toSet.subsetOf(allReferences)
  })

  def enrichDocuments(filesToAnalyze: Array[String], formatter: LatexFormatter): Array[DocumentInfo] = {
    require(filesToAnalyze.nonEmpty, "No files to analyze")
    val landoAnalyzer = LandoDocumentEnricher(formatter)
    val sysmlAnalyzer = SysMLDocumentEnricher(formatter)
    val cryptolAnalyzer = CryptolDocumentEnricher(formatter)

    val landoFilesToAnalyse = filesToAnalyze.filter(file => FileUtil.getFileType(file).equals("lando"))
    val sysmlFilesToAnalyse = filesToAnalyze.filter(file => FileUtil.getFileType(file).equals("sysml"))
    val cryptolFilesToAnalyse = filesToAnalyze.filter(file => FileUtil.getFileType(file).equals("cry"))

    val landoDocuments = landoFilesToAnalyse.map(landoAnalyzer.extractDocumentInfo)
    val sysMLDocuments = sysmlFilesToAnalyse.map(sysmlAnalyzer.extractDocumentInfo)
    val cryptolDocuments = cryptolFilesToAnalyse.map(cryptolAnalyzer.extractDocumentInfo)

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

    enrichedLandoDocuments ++ enrichedSysMLDocuments ++ enrichedCryptolDocuments
  } ensuring ((res: Array[DocumentInfo]) => res.length == filesToAnalyze.length)


  //  def cleanUpUnusedGlossaries(filesToAnalyze: Array[String]): Array[String] = {
  //    require(filesToAnalyze.nonEmpty)
  //
  //    val enrichedDocuments = enrichDocuments(filesToAnalyze)
  //    val nonAbstractedReferences = nonSpecializedLandoConstructs(enrichedDocuments)
  //    val enrichedLandoDocuments = FileUtil.getLandoDocuments(enrichedDocuments)
  //    val enrichedSysMLDocuments = FileUtil.getSysMLDocuments(enrichedDocuments)
  //
  //    val cleanedLandoDocuments = enrichedLandoDocuments.foldLeft(Array.empty[DocumentInfo])((documents, document) => {
  //      val updatedDocument = if (document.documentName.contains("glossary")) {
  //        val specializedReference = document.getAllReferences.filterNot(ref => nonAbstractedReferences.contains(ref))
  //        assert(specializedReference.size < document.getAllReferences.size)
  //        LandoDocumentInfo(
  //          document.documentName,
  //          document.filePath,
  //          specializedReference.filter(ref => Set(ReferenceType.Component, ReferenceType.System, ReferenceType.SubSystem).contains(ref.getReferenceType)),
  //          document.getRelations,
  //          specializedReference.filter(_.getReferenceType.equals(ReferenceType.Event)),
  //          specializedReference.filter(_.getReferenceType.equals(ReferenceType.Requirement)),
  //          specializedReference.filter(_.getReferenceType.equals(ReferenceType.Scenario))
  //        )
  //      } else {
  //        document
  //      }
  //      documents ++ Array[DocumentInfo](updatedDocument)
  //    })
  //
  //    val res = cleanedLandoDocuments.map(landoAnalyzer.enrichFile) ++ enrichedSysMLDocuments.map(sysmlAnalyzer.enrichFile)
  //    res
  //  } ensuring ((res: Array[String]) => res.length == filesToAnalyze.length)

}
