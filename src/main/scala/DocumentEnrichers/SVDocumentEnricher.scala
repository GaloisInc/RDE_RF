package DocumentEnrichers

import Formatter.LatexFormatter
import Types.DocReference.DocReference
import Types.DocumentInfos.{DocumentInfo, LobotDocumentInfo, SVDocumentInfo, SawDocumentInfo}
import Types.{DocumentType, ReferenceName, ReferenceType}
import Utils.{Control, FileUtil}
import org.apache.logging.log4j.scala.Logging

import scala.util.matching.Regex

class SVDocumentEnricher(override val formatterType: LatexFormatter) extends DocumentEnricher(formatterType) with Logging {
  // Reads a Document to create an object of the necessary information to enrich the document.
  val subsystemRegex: Regex = """^module\s+(\w+)\s*""".r

  def parseDocument(filePath: String): SVDocumentInfo = {
    require(filePath.nonEmpty, "File path cannot be empty")
    require(FileUtil.getFileType(filePath) == "sv", "File type must be SystemVerilog")
    require(FileUtil.fileExists(filePath), "filePath must exist")

    logger.info(s"Parsing file $filePath")

    val fileName = FileUtil.getFileName(filePath)
    val modules: Set[DocReference] = Control.extractReferences(filePath, (l: String) => transformReference(l, fileName))

    logger.info("Finished parsing file " + filePath)

    new SVDocumentInfo(fileName, filePath, modules)
  }

  def formatLine(line: String, documentInfo: DocumentInfo): String = {
    val references = documentInfo.getAllReferences
    line match {
      case subsystemRegex(_) => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.System))
      case _ => line
    }
  }

  def transformReference(line: String, fileName: String): Option[DocReference] = {
    cleanString(line) match {
      case subsystemRegex(systemName)
      => Some(new DocReference(fileName, ReferenceName(systemName, None), ReferenceType.System, DocumentType.SV, line))
      case _ => None
    }
  }

  private def cleanString(line: String): String = {
    line.trim()
  } ensuring ((res: String) => res.length <= line.length && line.contains(res))

}










