package DocumentEnrichers

import Formatter.LatexFormatter
import Specs.FileSpecs
import Types.DocumentInfos.SawDocumentInfo
import Utils.FileUtil
import org.apache.logging.log4j.scala.Logging

class SawDocumentEnricher(override val formatterType: LatexFormatter) extends DocumentEnricher[SawDocumentInfo](formatterType) with Logging {
  // Reads a Document to create an object of the necessary information to enrich the document.

  def parseDocument(filePath: String): SawDocumentInfo = {
    require(FileSpecs.fileChecks(Set(filePath), Set("saw")), "filePath must be a sysml file")
    logger.info(s"Parsing file $filePath")

    val fileName = FileUtil.fileNameFromPath(filePath)
    logger.info("Finished parsing file " + filePath)

    SawDocumentInfo(fileName, filePath)
  }
}
