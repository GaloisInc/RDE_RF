package Report.ReportTypes

import Report.PaperLayout
import Report.PaperLayout.PaperLayout
import Types.DocReference.DocReference
import Types.DocumentInfos.{CryptolDocumentInfo, DocumentInfo, LandoDocumentInfo, SysMLDocumentInfo}

case class ReportReference(title: String,
                           folder: String,
                           landoDocuments: Array[LandoDocumentInfo],
                           sysmlDocuments: Array[SysMLDocumentInfo],
                           cryptolDocuments: Array[CryptolDocumentInfo],
                           layout: PaperLayout,
                          ) {
  require(title.nonEmpty, "title must not be empty")
  require(folder.nonEmpty, "folder must not be empty")
  require(landoDocuments.nonEmpty || sysmlDocuments.nonEmpty || cryptolDocuments.nonEmpty, "At least one document must be provided")

  lazy val allDocuments: Array[DocumentInfo] = landoDocuments ++ sysmlDocuments ++ cryptolDocuments

  def allDocumentNamesToPaths: Map[String, String] = {
    val docMap = allDocuments.map(doc => doc.documentName -> doc.filePath).toMap
    docMap
  } //ensuring(_.size == allDocuments.length, "All documents must be present in the map")
  
  lazy val getAllReferences = allDocuments.flatMap(_.getAllReferences)

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


}
