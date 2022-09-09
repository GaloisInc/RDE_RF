package Report.ReportTypes

import Types.DocumentInfos.{CryptolDocumentInfo, DocumentInfo, LandoDocumentInfo, SysMLDocumentInfo}

case class ReportReference(title: String,
                           folder: String,
                           landoDocuments: Array[LandoDocumentInfo],
                           sysmlDocuments: Array[SysMLDocumentInfo],
                           cryptolDocuments: Array[CryptolDocumentInfo]
                          ) {
  require(title.nonEmpty, "title must not be empty")
  require(folder.nonEmpty, "folder must not be empty")
  require(landoDocuments.nonEmpty || sysmlDocuments.nonEmpty || cryptolDocuments.nonEmpty, "At least one document must be provided")

  lazy val allDocuments: Array[DocumentInfo] = landoDocuments ++ sysmlDocuments ++ cryptolDocuments

  def allDocumentNamesToPaths: Map[String, String] = {
    val docMap = allDocuments.map(doc => doc.documentName -> doc.filePath).toMap
    docMap
  } //ensuring(_.size == allDocuments.length, "All documents must be present in the map")
}
