package DocumentEnrichers

import Formatter.LatexFormatter
import Types.DocumentInfos.LobotDocumentInfo
import Utils.FileUtil
import org.apache.logging.log4j.scala.Logging

class LobotDocumentEnricher(override val formatterType: LatexFormatter) extends DocumentEnricher[LobotDocumentInfo](formatterType) with Logging {
  def parseDocument(filePath: String): LobotDocumentInfo = {
    require(filePath.nonEmpty, "File path cannot be empty")
    require(FileUtil.getFileType(filePath) == "lobot", "File type must be Lobot")
    require(FileUtil.fileExists(filePath), "filePath must exist")

    logger.info(s"Parsing file $filePath")

    val fileName = FileUtil.getFileName(filePath)
    logger.info("Finished parsing file " + filePath)

    new LobotDocumentInfo(fileName, filePath, Set(), Set(), Set(), Set(), Set())
  }

  def formatLine(line: String, documentInfo: LobotDocumentInfo): String = {
    line match {
      case _ => line
    }
  }
}
