package DocumentEnrichers

import Formatter.LatexFormatter
import Types.*
import Types.DocReference.DocReference
import Types.DocumentInfos.{CryptolDocumentInfo, DocumentInfo}
import Utils.FileUtil

import java.util.Locale
import scala.util.matching.Regex

class CryptolDocumentEnricher(override val formatterType: LatexFormatter,
                              override val skipTodos: Boolean = true) extends DocumentEnricher(formatterType, skipTodos) {
  // Reads a Document to create an object of the necessary information to enrich the document.
  val keyWordsToRemove: Array[String] = Array("private")

  //Cryptol
  val keyWordsToReference: ReferenceKeyWords = ReferenceKeyWords(
    Type = "type",
    Import = "import",
    Requirement = "property",
  )

  def extractDocumentInfo(filePath: String): CryptolDocumentInfo = {
    require(filePath.nonEmpty)
    require(FileUtil.getFileType(filePath) == "cry")
    val fileName = FileUtil.getFileName(filePath)
    val types: Set[DocReference] = extractReferences(filePath, ReferenceType.Type)
    val properties: Set[DocReference] = extractReferences(filePath, ReferenceType.Requirement)
    val functions: Set[DocReference] = extractReferences(filePath, ReferenceType.Event)
    val imports: Set[DocReference] = Set.empty[DocReference]

    CryptolDocumentInfo(fileName, filePath, imports, types, functions, properties)
  }

  def formatLine(line: String, documentInfo: DocumentInfo): String = {
    val references = documentInfo.getAllReferences
    cryotolType(line) match
      case Some(value) => value match
        case ReferenceType.Event => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.Event))
        case ReferenceType.Requirement => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.Requirement))
        case ReferenceType.Import => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.Import))
        case ReferenceType.Type => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.Type))
        case _ => line
      case None => line
  }

  def transformReference(line: String, fileName: String, fileType: FileType): DocReference = {
    val strippedLine = removeCryptolMetaData(line)
    val cryptolType = cryotolType(strippedLine).get
    val name = extractTypeDependency(line, cryptolType)
    val referenceInfo = ReferenceName(name)

    DocReference(
      fileName,
      referenceInfo,
      cryptolType,
      DocumentType.Cryptol,
      line
    )
  }

  private def extractReferences(filePath: String, getReferenceType: ReferenceType): Set[DocReference] = {
    extract(filePath, (line: String, _: String) => filterReferenceTypes(line, getReferenceType), transformReference)
  } ensuring ((references: Set[DocReference]) => references.forall(ref => ref.getReferenceType == getReferenceType && ref.getDocumentType == DocumentType.Cryptol))

  def extractTypeDependency(str: String, getReferenceType: ReferenceType): String = {
    val cleanLine = removeCryptolMetaData(str)
    val name = getReferenceType match
      case ReferenceType.Requirement => cleanLine.replace("property", "").strip().split(" ").head
      case ReferenceType.Event => cleanLine.split(":").head
      case ReferenceType.Import => cleanLine.replace("import", "")
      case ReferenceType.Type => cleanLine.replace("type", "").strip().split("=").head
      case _ => cleanLine
    extractName(name.strip(), getReferenceType)
  }

  def extractName(str: String, getReferenceType: ReferenceType): String = {
    val strippedLine = str.replace(getReferenceType.toString.toLowerCase, "").strip()
    strippedLine
  }

  private def cryotolType(line: String): Option[ReferenceType] = {
    val lowerCaseLine = removeCryptolMetaData(line).toLowerCase(Locale.US).strip()
    if lowerCaseLine.startsWith("property") then
      Some(ReferenceType.Requirement)
    else if lowerCaseLine.startsWith("import") then
      Some(ReferenceType.Import)
    else if lowerCaseLine.startsWith("type") then
      Some(ReferenceType.Type)
    else if lowerCaseLine.contains("->") && lowerCaseLine.contains(":") && !lowerCaseLine.contains("::")
    then
      Some(ReferenceType.Event)
    else
      None
  }

  private def filterReferenceTypes(line: String, getReferenceType: ReferenceType): Boolean = {
    val strippedLine = line.strip()
    strippedLine.nonEmpty
      && !strippedLine.startsWith("//")
      && !strippedLine.startsWith("/*")
      && cryotolType(strippedLine).nonEmpty
      && cryotolType(strippedLine).get == getReferenceType
  }

  private def removeCryptolMetaData(line: String): String = {
    line.strip()
  } ensuring ((res: String) => res.length <= line.length)

  def getFileType(path: String): FileType = {
    FileType.ComponentFile
  }
}
