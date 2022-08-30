import DocumentEnrichers.{FileUtil, LandoDocumentEnricher, SysMLDocumentEnricher, CryptolDocumentEnricher}
import Referencer.{LandoReferencer, SysMLReferencer, CryptolReferencer}
import Types.{DocReference, DocumentInfo, ReferenceType, DocumentType, LandoDocumentInfo}

object DocumentAnalyzer {
  //Analyzers
  private val landoAnalyzer = LandoDocumentEnricher()
  private val sysmlAnalyzer = SysMLDocumentEnricher()
  private val cryptolAnalyzer = CryptolDocumentEnricher()
  //Referencers
  private val landoReferencer = LandoReferencer()
  private val sysMLReferencer = SysMLReferencer()
  private val cryptolReferencer = CryptolReferencer()

  private val fileUtil = FileUtil()

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


  def nonSpecializedLandoConstructs(filesToAnalyze: Array[String]): Array[DocReference] = {
    require(filesToAnalyze.nonEmpty)

    val enrichedDocuments = enrichDocuments(filesToAnalyze)
    val enrichedLandoDocuments = fileUtil.getLandoDocuments(enrichedDocuments)

    val nonSpecializedConstructs = enrichedLandoDocuments.flatMap(doc => doc.getAllReferences.filter(ref => ref.abstracts.isEmpty))
    nonSpecializedConstructs
  }

  def nonSpecializedLandoConstructs(documents: Array[DocumentInfo]): Array[DocReference] = {
    require(documents.nonEmpty)
    val enrichedLandoDocuments = fileUtil.getLandoDocuments(documents)
    val nonSpecializedConstructs = enrichedLandoDocuments.flatMap(doc => doc.getAllReferences.filter(ref => ref.abstracts.isEmpty))
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
          specializedReference.filter(ref => Set(ReferenceType.Component, ReferenceType.System, ReferenceType.SubSystem).contains(ref.referenceType)),
          document.getRelations,
          specializedReference.filter(_.referenceType.equals(ReferenceType.Event)),
          specializedReference.filter(_.referenceType.equals(ReferenceType.Requirement)),
          specializedReference.filter(_.referenceType.equals(ReferenceType.Scenario))
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
    assert(landoDocuments.flatMap(_.getAllReferences.map(_.referenceName.reference)).toSet.size == landoDocuments.flatMap(_.getAllReferences.map(_.referenceName.reference)).length)
    assert(cryptolDocuments.flatMap(_.getAllReferences.map(_.referenceName.reference)).toSet.size == cryptolDocuments.flatMap(_.getAllReferences.map(_.referenceName.reference)).length)
    assert(sysMLDocuments.flatMap(_.getAllReferences.map(_.referenceName.reference)).toSet.size == sysMLDocuments.flatMap(_.getAllReferences.map(_.referenceName.reference)).length)

    val enrichedCryptolDocuments = cryptolDocuments.map(doc => cryptolReferencer.addSpecializationAndAbstract(doc, sysMLDocuments.map(_.asInstanceOf[DocumentInfo]), Array.empty[DocumentInfo]))
    val enrichedSysMLDocuments = sysMLDocuments.map(doc => sysMLReferencer.addSpecializationAndAbstract(doc, landoDocuments.map(_.asInstanceOf[DocumentInfo]), enrichedCryptolDocuments))
    val enrichedLandoDocuments = landoDocuments.map(doc => landoReferencer.addSpecializationAndAbstract(doc, Array.empty[DocumentInfo], enrichedSysMLDocuments))

    val res = enrichedLandoDocuments ++ enrichedSysMLDocuments ++ enrichedCryptolDocuments
    res
  } ensuring ((res: Array[DocumentInfo]) => res.length == filesToAnalyze.length)
}