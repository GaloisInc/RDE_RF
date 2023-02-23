package DocumentEnrichers

import Formatter.LatexFormatter
import Specs.FileSpecs
import Types.DocReference.DocReference
import Types.DocumentInfos.SVDocumentInfo
import Types.{DocumentType, ReferenceName, ReferenceType}
import Utils.{Control, FileUtil}
import org.apache.logging.log4j.scala.Logging

import scala.util.matching.Regex

class SVDocumentEnricher(override val formatterType: LatexFormatter) extends DocumentEnricher[SVDocumentInfo](formatterType) with Logging {
  // Reads a Document to create an object of the necessary information to enrich the document.
  val subsystemRegex: Regex = """^module\s+(\w+)\s*""".r

  def parseDocument(filePath: String): SVDocumentInfo = {
    require(FileSpecs.fileChecks(Set(filePath), Set("sv")), "filePath must be a sysml file")

    logger.info(s"Parsing file $filePath")

    val fileName = FileUtil.fileNameFromPath(filePath)
    val modules: Set[DocReference] = Control.extractReferences(filePath, (l: String) => transformReference(l, fileName))

    logger.info("Finished parsing file " + filePath)

    new SVDocumentInfo(fileName, filePath, modules)
  }

  override def formatLine(line: String, documentInfo: SVDocumentInfo): String = {
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










