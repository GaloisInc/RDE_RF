package Report.ReportTypes

import Types.DocumentInfos.{CryptolDocumentInfo, LandoDocumentInfo, SysMLDocumentInfo}

case class ReportReference(title: String,
                           folder: String,
                           landoDocuments: Array[LandoDocumentInfo],
                           sysmlDocuments: Array[SysMLDocumentInfo],
                           cryptolDocuments: Array[CryptolDocumentInfo]
                          ){
  require(title.nonEmpty, "title must not be empty")
  require(folder.nonEmpty, "folder must not be empty")
  require(landoDocuments.nonEmpty || sysmlDocuments.nonEmpty || cryptolDocuments.nonEmpty, "At least one document must be provided")
}
