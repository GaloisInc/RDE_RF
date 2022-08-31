package DocumentEnrichers

import Types.*
import Types.DocumentInfos.{DocumentInfo, SVDocumentInfo}

import java.util.Locale
import scala.util.matching.Regex

class SVDocumentEnricher extends DocumentEnricher {
  // Reads a Document to create an object of the necessary information to enrich the document.
  val keyWordsToRemove: Array[String] = Array.empty

  val keyWordsToReference: ReferenceKeyWords = ReferenceKeyWords(
    System = "module",
    //Input?
    //Outputs?
  )

  def extractDocumentInfo(filePath: String): SVDocumentInfo = {
    require(filePath.nonEmpty)
    require(fileUtil.getFileType(filePath) == "sv")
    val fileName = fileUtil.getFileName(filePath)
    val modules: Set[DocReference] = extractReferences(filePath, ReferenceType.System)

    SVDocumentInfo(fileName, filePath, modules)
  }

  def formatLine(line: String, documentInfo: DocumentInfo): String = {
    val references = documentInfo.getAllReferences
    getReferenceType(line) match
      case Some(value) => value match
        case ReferenceType.System => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.System))
        case _ => line
      case None => line
  }

  def transformReference(line: String, fileName: String, fileType: FileType): DocReference = {
    val referenceType = getReferenceType(line).get
    val name = extractModuleName(line, referenceType)
    val reference = referenceText(name, s"sv_${fileName}_${referenceType.toString}")
    val referenceInfo = ReferenceName(name, reference)

    DocReference(
      fileName,
      referenceInfo,
      referenceType,
      DocumentType.SV,
      line,
      Some(latexFormatter.enrichLineWithLabel(line, reference))
    )
  }

  private def extractReferences(filePath: String, referenceType: ReferenceType): Set[DocReference] = {
    extract(filePath, (line: String, _: String) => filterReferenceTypes(line, referenceType), transformReference)
  } ensuring ((references: Set[DocReference]) => references.forall(ref => ref.referenceType == referenceType && ref.documentType == DocumentType.SV))

  def referenceText(name: String, typeString: String): String = {
    val sanitizedName = latexFormatter.sanitizeLine(name)
    s"${typeString}_$sanitizedName"
  }

  def extractModuleName(str: String, referenceType: ReferenceType): String = {
    val cleanLine = trimString(str)
    val name = referenceType match
      case ReferenceType.System => cleanLine.replace("module", "").strip()
      case _ => cleanLine
    name
  }

  private def filterReferenceTypes(line: String, referenceType: ReferenceType): Boolean = {
    val strippedLine = line.strip()
    strippedLine.nonEmpty
      && !strippedLine.startsWith("//")
      && !strippedLine.startsWith("/*")
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
