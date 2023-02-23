package DocumentEnrichers

import Formatter.LatexFormatter
import Specs.FileSpecs
import Types.DocumentInfos.LobotDocumentInfo
import Utils.FileUtil
import org.apache.logging.log4j.scala.Logging

class LobotDocumentEnricher(override val formatterType: LatexFormatter) extends DocumentEnricher[LobotDocumentInfo](formatterType) with Logging {
  def parseDocument(filePath: String): LobotDocumentInfo = {
    require(FileSpecs.fileChecks(Set(filePath), Set("lobot")), "filePath must be a sysml file")
    logger.info(s"Parsing file $filePath")

    val fileName = FileUtil.fileNameFromPath(filePath)
    logger.info("Finished parsing file " + filePath)

    new LobotDocumentInfo(fileName, filePath, Set(), Set(), Set(), Set(), Set())
  }
}
