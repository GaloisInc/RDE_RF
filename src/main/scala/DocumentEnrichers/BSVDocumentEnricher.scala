package DocumentEnrichers

import Formatter.LatexFormatter
import Types.*
import Types.DocReference.DocReference
import Types.DocumentInfos.{BSVDocumentInfo, DocumentInfo}
import Utils.FileUtil

import java.util.Locale
import scala.util.matching.Regex

class BSVDocumentEnricher(override val formatterType: LatexFormatter,
                          override val skipTodos: Boolean = true) extends DocumentEnricher {
  // Reads a Document to create an object of the necessary information to enrich the document.
  val keyWordsToRemove: Array[String] = Array.empty

  val keyWordsToReference: ReferenceKeyWords = ReferenceKeyWords(
    System = "package",
    SubSystem = "module",
    //Imports?
  )

  def extractDocumentInfo(filePath: String): BSVDocumentInfo = {
    require(filePath.nonEmpty)
    require(FileUtil.getFileType(filePath) == "bsv")
    val fileName = FileUtil.getFileName(filePath)
    val packages: Set[DocReference] = extractReferences(filePath, ReferenceType.System)
    val modules: Set[DocReference] = extractReferences(filePath, ReferenceType.SubSystem)

    BSVDocumentInfo(fileName, filePath, packages, modules)
  }

  def formatLine(line: String, documentInfo: DocumentInfo): String = {
    val references = documentInfo.getAllReferences
    getReferenceType(line) match
      case Some(value) => value match
        case ReferenceType.System => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.System))
        case ReferenceType.SubSystem => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.SubSystem))
        case _ => line
      case None => line
  }

  def transformReference(line: String, fileName: String, fileType: FileType): DocReference = {
    val referenceType = getReferenceType(line).get
    val name = extractModuleName(line, referenceType)
    val referenceInfo = ReferenceName(name)

    DocReference(
      fileName,
      referenceInfo,
      referenceType,
      DocumentType.BSV,
      line
    )
  }

  private def extractReferences(filePath: String, getReferenceType: ReferenceType): Set[DocReference] = {
    extract(filePath, (line: String, _: String) => filterReferenceTypes(line, getReferenceType), transformReference)
  } ensuring ((references: Set[DocReference]) =>
    references.forall(ref =>
      ref.getReferenceType == getReferenceType
      && ref.getDocumentType == DocumentType.BSV))


  def extractModuleName(str: String, getReferenceType: ReferenceType): String = {
    val cleanLine = trimString(str)
    val name = getReferenceType match
      case ReferenceType.SubSystem => cleanLine.replace("module", "").strip()
      case ReferenceType.System => cleanLine.replace("package", "").strip()
      case _ => cleanLine
    name
  }

  private def filterReferenceTypes(line: String, referenceType: ReferenceType): Boolean = {
    val strippedLine = line.strip()
    strippedLine.nonEmpty
      && getReferenceType(strippedLine).nonEmpty
      && getReferenceType(strippedLine).get == referenceType
  }

  private def trimString(line: String): String = {
    line.toLowerCase(Locale.US).strip()
  } ensuring ((res: String) => res.length <= line.length)

  def getFileType(path: String): FileType = {
    FileType.ComponentFile
  }
}
