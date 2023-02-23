package Referencer

import Types.DocumentInfos.{CryptolDocumentInfo, FretDocument, LandoDocumentInfo, SysMLDocumentInfo}

object FretSysMLReferencer extends Referencer[FretDocument, LandoDocumentInfo, SysMLDocumentInfo] {}
object FretCryptolReferencer extends Referencer[FretDocument, LandoDocumentInfo, CryptolDocumentInfo] {}
