package DocumentEnrichers

import Types.*
import Types.DocumentInfos.{CryptolDocumentInfo, DocumentInfo}

import java.util.Locale
import scala.util.matching.Regex

class CDocumentEnricher extends DocumentEnricher {
  // Reads a Document to create an object of the necessary information to enrich the document.
  val keyWordsToRemove: Array[String] = Array("private", "public", "static")


  //Cryptol
  val keyWordsToReference: ReferenceKeyWords = ReferenceKeyWords(
    Type = "type",
    Import = "import",
    Requirement = "property",
  )


  def extractDocumentInfo(filePath: String): CryptolDocumentInfo = {
    require(filePath.nonEmpty)
    require(fileUtil.getFileType(filePath) == "cry")
    val fileName = fileUtil.getFileName(filePath)
    val types: Set[DocReference] = extractReferences(filePath, ReferenceType.Type)
    val properties: Set[DocReference] = extractReferences(filePath, ReferenceType.Requirement)
    val functions: Set[DocReference] = extractReferences(filePath, ReferenceType.Event)
    val imports: Set[DocReference] = Set.empty[DocReference]

    CryptolDocumentInfo(fileName, filePath, imports, types, functions, properties)
  }

  def formatLine(line: String, documentInfo: DocumentInfo): String = {
    val references = documentInfo.getAllReferences
    getReferenceType(line) match
      case Some(value) => value match
        case ReferenceType.Event => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.Event))
        case ReferenceType.Requirement => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.Requirement))
        case ReferenceType.Import => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.Import))
        case ReferenceType.Type => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.Type))
        case _ => line
      case None => line
  }

  def transformReference(line: String, fileName: String, fileType: FileType): DocReference = {
    val strippedLine = removeCryptolMetaData(line)
    val cryptolType = cryotolType(strippedLine).get
    val name = extractTypeDependency(line, cryptolType)
    val reference = referenceText(name, s"cryptol_${fileName}_${cryptolType.toString}")
    val referenceInfo = ReferenceName(name, reference)

    DocReference(
      fileName,
      referenceInfo,
      cryptolType,
      DocumentType.Cryptol,
      line,
      Some(latexFormatter.enrichLineWithLabel(line, reference))
    )
  }

  private def extractReferences(filePath: String, referenceType: ReferenceType): Set[DocReference] = {
    extract(filePath, (line: String, _: String) => filterReferenceTypes(line, referenceType), transformReference)
  } ensuring ((references: Set[DocReference]) => references.forall(ref => ref.referenceType == referenceType && ref.documentType == DocumentType.Cryptol))

  def referenceText(name: String, typeString: String): String = {
    val sanitizedName = latexFormatter.sanitizeLine(name)
    s"${typeString}_$sanitizedName"
  }

  def extractTypeDependency(str: String, referenceType: ReferenceType): String = {
    val cleanLine = removeCryptolMetaData(str)
    val name = referenceType match
      case ReferenceType.Requirement => cleanLine.replace("property", "").strip().split(" ").head
      case ReferenceType.Event => cleanLine.split(":").head
      case ReferenceType.Import => cleanLine.replace("import", "")
      case ReferenceType.Type => cleanLine.replace("type", "").strip().split("=").head
      case _ => cleanLine
    extractName(name.strip(), referenceType)
  }

  def extractName(str: String, referenceType: ReferenceType): String = {
    val strippedLine = str.replace(referenceType.toString.toLowerCase, "").strip()
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

  private def filterReferenceTypes(line: String, referenceType: ReferenceType): Boolean = {
    val strippedLine = line.strip()
    strippedLine.nonEmpty
      && !strippedLine.startsWith("//")
      && !strippedLine.startsWith("/*")
      && cryotolType(strippedLine).nonEmpty
      && cryotolType(strippedLine).get == referenceType
  }

  private def removeCryptolMetaData(line: String): String = {
    line.strip()
  } ensuring ((res: String) => res.length <= line.length)

  def getFileType(path: String): FileType = {
    FileType.ComponentFile
  }
}
