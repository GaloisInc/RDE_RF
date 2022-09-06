package Analyzers

import Types.DocumentInfos.{DocumentInfo, LandoDocumentInfo}
import Types.{DocReference, ReferenceType}
import DocumentEnrichers.*
import Referencer.*
import Utils.*
import Formatter.InlineFormatter

import java.nio.file.Path

object DocumentAnalyzer {
  private val formatterType = InlineFormatter()
  //Analyzers
  private val landoAnalyzer = LandoDocumentEnricher(formatterType)
  private val sysmlAnalyzer = SysMLDocumentEnricher(formatterType)
  private val cryptolAnalyzer = CryptolDocumentEnricher(formatterType)
  //Referencers
  private val landoReferencer = LandoReferencer()
  private val sysMLReferencer = SysMLReferencer()
  private val cryptolReferencer = CryptolReferencer()

  private val fileUtil = FileUtil()

  def enrichAndSortFiles(filesToAnalyze: Array[String]): Array[String] = {
    val enrichedFiles = enrichFiles(filesToAnalyze)
    val decoratedLandoFiles = enrichedFiles.filter(_.endsWith("lando"))
    val decoratedSysMLFiles = enrichedFiles.filter(_.endsWith("sysml"))
    val decoratedCryptolFiles = enrichedFiles.filter(_.endsWith("cry"))

    val newLandoFiles = decoratedLandoFiles.map(filePath => {
      val destinationPath = Path.of(fileUtil.getDirectory(filePath), "decoratedLando").toString
      fileUtil.moveRenameFile(filePath, destinationPath)
    })
    val newSysMLFiles = decoratedSysMLFiles.map(filePath => {
      val destinationPath = Path.of(fileUtil.getDirectory(filePath), "decoratedSysML").toString
      fileUtil.moveRenameFile(filePath, destinationPath)
    })

    val newCryptolFiles = decoratedCryptolFiles.map(filePath => {
      val destinationPath = Path.of(fileUtil.getDirectory(filePath), "decoratedCryptol").toString
      fileUtil.moveRenameFile(filePath, destinationPath)
    })

    newLandoFiles ++ newSysMLFiles ++ newCryptolFiles
  } ensuring ((res: Array[String]) => res.length == filesToAnalyze.length)

  def enrichFiles(filesToAnalyze: Array[String]): Array[String] = {
    require(filesToAnalyze.nonEmpty)

    val enrichedDocuments = enrichDocuments(filesToAnalyze)

    val enrichedLandoDocuments = fileUtil.getLandoDocuments(enrichedDocuments)
    val enrichedSysMLDocuments = fileUtil.getSysMLDocuments(enrichedDocuments)
    val enrichedCryptolDocuments = fileUtil.getCryptolDocuments(enrichedDocuments)

    assert(enrichedCryptolDocuments.intersect(enrichedSysMLDocuments).isEmpty)
    assert(enrichedLandoDocuments.intersect(enrichedSysMLDocuments).isEmpty)
    assert(enrichedCryptolDocuments.intersect(enrichedLandoDocuments).isEmpty)

    val res = enrichedLandoDocuments.map(landoAnalyzer.enrichFile)
      ++ enrichedSysMLDocuments.map(sysmlAnalyzer.enrichFile)
      ++ enrichedCryptolDocuments.map(cryptolAnalyzer.enrichFile)
    res
  } ensuring ((res: Array[String]) => res.length == filesToAnalyze.length)


  def nonRefinementLando(filesToAnalyze: Array[String]): Array[DocReference] = {
    require(filesToAnalyze.nonEmpty)

    val enrichedDocuments = enrichDocuments(filesToAnalyze)
    val enrichedLandoDocuments = fileUtil.getLandoDocuments(enrichedDocuments)

    val nonSpecializedConstructs = enrichedLandoDocuments.flatMap(doc => doc.getAllReferences.filter(ref => ref.getAbstracts.isEmpty))
    nonSpecializedConstructs
  }

  def nonSpecializedLandoConstructs(documents: Array[DocumentInfo]): Array[DocReference] = {
    require(documents.nonEmpty)
    val enrichedLandoDocuments = fileUtil.getLandoDocuments(documents)
    val nonSpecializedConstructs = enrichedLandoDocuments.flatMap(doc => doc.getAllReferences.filter(ref => ref.getAbstracts.isEmpty))
    nonSpecializedConstructs
  } ensuring ((nonSpecialized: Array[DocReference]) => {
    val allReferences = documents.flatMap(doc => doc.getAllReferences).toSet
    nonSpecialized.toSet.subsetOf(allReferences)
  })

  def cleanUpUnusedGlossaries(filesToAnalyze: Array[String]): Array[String] = {
    require(filesToAnalyze.nonEmpty)

    val enrichedDocuments = enrichDocuments(filesToAnalyze)
    val nonAbstractedReferences = nonSpecializedLandoConstructs(enrichedDocuments)
    val enrichedLandoDocuments = fileUtil.getLandoDocuments(enrichedDocuments)
    val enrichedSysMLDocuments = fileUtil.getSysMLDocuments(enrichedDocuments)

    val cleanedLandoDocuments = enrichedLandoDocuments.foldLeft(Array.empty[DocumentInfo])((documents, document) => {
      val updatedDocument = if (document.documentName.contains("glossary")) {
        val specializedReference = document.getAllReferences.filterNot(ref => nonAbstractedReferences.contains(ref))
        assert(specializedReference.size < document.getAllReferences.size)
        LandoDocumentInfo(
          document.documentName,
          document.filePath,
          specializedReference.filter(ref => Set(ReferenceType.Component, ReferenceType.System, ReferenceType.SubSystem).contains(ref.getReferenceType)),
          document.getRelations,
          specializedReference.filter(_.getReferenceType.equals(ReferenceType.Event)),
          specializedReference.filter(_.getReferenceType.equals(ReferenceType.Requirement)),
          specializedReference.filter(_.getReferenceType.equals(ReferenceType.Scenario))
        )
      } else {
        document
      }
      documents ++ Array[DocumentInfo](updatedDocument)
    })

    val res = cleanedLandoDocuments.map(landoAnalyzer.enrichFile) ++ enrichedSysMLDocuments.map(sysmlAnalyzer.enrichFile)
    res
  } ensuring ((res: Array[String]) => res.length == filesToAnalyze.length)

  def enrichDocuments(filesToAnalyze: Array[String]): Array[DocumentInfo] = {
    val landoFilesToAnalyse = filesToAnalyze.filter(file => fileUtil.getFileType(file).equals("lando"))
    val sysmlFilesToAnalyse = filesToAnalyze.filter(file => fileUtil.getFileType(file).equals("sysml"))
    val cryptolFilesToAnalyse = filesToAnalyze.filter(file => fileUtil.getFileType(file).equals("cry"))

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

    val enrichedCryptolDocuments = cryptolDocuments.map(doc => cryptolReferencer.addSpecializationAndAbstract(doc, sysMLDocuments.map(_.asInstanceOf[DocumentInfo]), Array.empty[DocumentInfo]))
    val enrichedSysMLDocuments = sysMLDocuments.map(doc => sysMLReferencer.addSpecializationAndAbstract(doc, landoDocuments.map(_.asInstanceOf[DocumentInfo]), enrichedCryptolDocuments))
    val enrichedLandoDocuments = landoDocuments.map(doc => landoReferencer.addSpecializationAndAbstract(doc, Array.empty[DocumentInfo], enrichedSysMLDocuments))

    enrichedLandoDocuments ++ enrichedSysMLDocuments ++ enrichedCryptolDocuments
  } ensuring ((res: Array[DocumentInfo]) => res.length == filesToAnalyze.length)
}
