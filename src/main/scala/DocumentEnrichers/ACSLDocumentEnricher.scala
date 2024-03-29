package DocumentEnrichers

import Formatter.LatexFormatter
import Specs.FileSpecs
import Types.DocumentInfos.CDocumentInfo
import Utils.FileUtil
import org.apache.logging.log4j.scala.Logging

class ACSLDocumentEnricher(override val formatterType: LatexFormatter,
                           override val skipTodos: Boolean = true) extends DocumentEnricher[CDocumentInfo](formatterType, skipTodos) with Logging {

  def parseDocument(filePath: String): CDocumentInfo = {
    require(FileSpecs.fileChecks(Set(filePath), Set("c", "h")), "filePath must be a c file")

    val fileName = FileUtil.fileNameFromPath(filePath)
    logger.info(s"Parsing file $filePath")

    CDocumentInfo(fileName, filePath)
  }
}
