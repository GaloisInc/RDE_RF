package DocumentEnrichers

import Formatter.LatexFormatter
import Types.DocumentInfos.{CDocumentInfo, DocumentInfo}
import Utils.FileUtil
import org.apache.logging.log4j.scala.Logging

class ACSLDocumentEnricher(override val formatterType: LatexFormatter,
                           override val skipTodos: Boolean = true) extends DocumentEnricher(formatterType, skipTodos) with Logging  {

  def parseDocument(filePath: String): CDocumentInfo = {
    require(filePath.nonEmpty, "filePath must not be empty")
    require(FileUtil.getFileType(filePath) == "c" ||
      FileUtil.getFileType(filePath) == "h", "filePath must be a c source file or a header file")
    require(FileUtil.fileExists(filePath), "filePath must exist")

    val fileName = FileUtil.getFileName(filePath)
    logger.info(s"Parsing file $filePath")


    new CDocumentInfo(fileName, filePath)
  }


  def formatLine(line: String, documentInfo: DocumentInfo): String = {
    val references = documentInfo.getAllReferences
    line match {
      case _ => line
    }
  }

}
