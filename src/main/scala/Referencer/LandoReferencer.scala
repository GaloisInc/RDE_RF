package Referencer

import Types.DocumentInfos.{FretDocument, LandoDocumentInfo, SysMLDocumentInfo}

object LandoSysMLReferencer extends Referencer[LandoDocumentInfo, LandoDocumentInfo, SysMLDocumentInfo] {}
object LandoFretReferencer extends Referencer[LandoDocumentInfo, LandoDocumentInfo, FretDocument] {}