package Report.ReportTypes

import Types.DocumentInfos.{CryptolDocumentInfo, LandoDocumentInfo, SysMLDocumentInfo}

case class ReportReference(title: String,
                           landoDocuments: Array[LandoDocumentInfo],
                           sysmlDocuments: Array[SysMLDocumentInfo],
                           cryptolDocuments: Array[CryptolDocumentInfo]
                          )
