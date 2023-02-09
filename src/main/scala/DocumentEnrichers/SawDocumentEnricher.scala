package DocumentEnrichers

import Formatter.LatexFormatter
import Types.DocumentInfos.{DocumentInfo, SawDocumentInfo}
import Utils.FileUtil
import org.apache.logging.log4j.scala.Logging

class SawDocumentEnricher(override val formatterType: LatexFormatter) extends DocumentEnricher[SawDocumentInfo](formatterType) with Logging {
  // Reads a Document to create an object of the necessary information to enrich the document.

  def parseDocument(filePath: String): SawDocumentInfo = {
    require(filePath.nonEmpty, "File path cannot be empty")
    require(FileUtil.getFileType(filePath) == "saw", "File type must be Saw")
    require(FileUtil.fileExists(filePath), "filePath must exist")

    logger.info(s"Parsing file $filePath")

    val fileName = FileUtil.getFileName(filePath)
    logger.info("Finished parsing file " + filePath)

    new SawDocumentInfo(fileName, filePath)
  }

  def formatLine(line: String, documentInfo: SawDocumentInfo): String = {
    val references = documentInfo.getAllReferences
    line match {
      case _ => line
    }
  }
}
