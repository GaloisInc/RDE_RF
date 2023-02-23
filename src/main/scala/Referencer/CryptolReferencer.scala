package Referencer

import Types.DocumentInfos.{BSVDocumentInfo, CryptolDocumentInfo, FretDocument, SVDocumentInfo, SysMLDocumentInfo}

object CryptolBSVReferencer extends Referencer[CryptolDocumentInfo, SysMLDocumentInfo, BSVDocumentInfo] {}

object CryptolSVReferencer extends Referencer[CryptolDocumentInfo, FretDocument, SVDocumentInfo] {}