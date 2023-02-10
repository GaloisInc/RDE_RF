package DocumentEnrichers

import Formatter.LatexFormatter
import Types.DocReference.DocReference
import Types.DocumentInfos.BSVDocumentInfo
import Types.{DocumentType, ReferenceName, ReferenceType}
import Utils.{Control, FileUtil}

import scala.util.matching.Regex

class BSVDocumentEnricher(override val formatterType: LatexFormatter,
                          override val skipTodos: Boolean = true) extends DocumentEnricher[BSVDocumentInfo](formatterType, skipTodos) {

  val systemRegex: Regex = """^package\s+(\w+)\s*""".r
  val subsystemRegex: Regex = """^module\s+(\w+)\s*\((\w+)\)\s*""".r
  val methodRegex: Regex = """^method\s+(\w+)\s*""".r
  val ruleRegex: Regex = """^rule\s+(\w+)\s*;""".r
  val actionRegex: Regex = """^action\s+(\w+)\s*""".r
  val interfaceRegex: Regex = """^interface\s+(\w+)\s*""".r

  def parseDocument(filePath: String): BSVDocumentInfo = {
    require(filePath.nonEmpty, "filePath must not be empty")
    require(FileUtil.getFileType(filePath) == "bsv", "filePath must be a BSV file")
    require(FileUtil.fileExists(filePath), "filePath must exist")

    val fileName = FileUtil.getFileName(filePath)
    val references = Control.extractReferences(filePath, (l: String) => transformReference(l, fileName))
    val packages = references.filter(_.getReferenceType == ReferenceType.System)
    val modules = references.filter(_.getReferenceType == ReferenceType.SubSystem)

    new BSVDocumentInfo(fileName, filePath, packages, modules)
  }

  def formatLine(line: String, documentInfo: BSVDocumentInfo): String = {
    val references = documentInfo.getAllReferences
    cleanLine(line) match {
      case systemRegex(_) => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.System))
      case subsystemRegex(_, _) => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.SubSystem))
      case _ => line
    }
  }

  def transformReference(line: String, fileName: String): Option[DocReference] = {
    cleanLine(line) match {
      case systemRegex(systemName) => Some(new DocReference(fileName, ReferenceName(systemName), ReferenceType.System, DocumentType.BSV, line))
      case subsystemRegex(subsystemName, _) => Some(new DocReference(fileName, ReferenceName(subsystemName), ReferenceType.SubSystem, DocumentType.BSV, line))
      case _ => None
    }
  }

  def cleanLine(line: String): String = {
    line.trim.stripSuffix(";").stripSuffix(",").stripSuffix("]").stripSuffix("}").stripSuffix("{")
  } ensuring((cleanedLine: String) => line.contains(cleanedLine), "cleanLine must not change the line")
}
