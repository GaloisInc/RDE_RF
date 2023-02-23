package DocumentEnrichers

import Formatter.LatexFormatter
import Specs.FileSpecs
import Types.DocumentInfos.{DocumentInfo, SawDocumentInfo}
import Utils.FileUtil
import org.apache.logging.log4j.scala.Logging

class SawDocumentEnricher(override val formatterType: LatexFormatter) extends DocumentEnricher[SawDocumentInfo](formatterType) with Logging {
  // Reads a Document to create an object of the necessary information to enrich the document.

  def parseDocument(filePath: String): SawDocumentInfo = {
    require(FileSpecs.fileChecks(Set(filePath), Set("saw")), "filePath must be a sysml file")

    logger.info(s"Parsing file $filePath")

    val fileName = FileUtil.fileNameFromPath(filePath)
    logger.info("Finished parsing file " + filePath)

    new SawDocumentInfo(fileName, filePath)
  }

  override def formatLine(line: String, documentInfo: SawDocumentInfo): String = {
    val references = documentInfo.getAllReferences
    line match {
      case _ => line
    }
  }
}
