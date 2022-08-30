package DocumentEnrichers

import Types.*

import scala.util.matching.Regex

class CryptolDocumentEnricher extends DocumentEnricher {
  // Reads a Document to create an object of the necessary information to enrich the document.
  def extractDocumentInfo(filePath: String): CryptolDocumentInfo = {
    require(filePath.nonEmpty)
    require(fileUtil.getFileType(filePath) == "cry")
    val fileName = fileUtil.getFileName(filePath)
    val types: Set[DocReference] = extractTypes(filePath)
    val properties: Set[DocReference] = extractProperties(filePath)
    val functions: Set[DocReference] = extractActions(filePath)
    val imports: Set[DocReference] = Set.empty[DocReference]

    CryptolDocumentInfo(fileName, filePath, imports, types, functions, properties)
  }

  def formatLine(line: String, documentInfo: DocumentInfo): String = {
    def searchCriteria = (ref: DocReference, srcLine: String) => ref.originalLine == srcLine

    def extractor = (ref: DocReference) => ref.enrichedLine.get

    val references = documentInfo.getAllReferences

    cryotolType(line) match
      case Some(value) => value match
        case ReferenceType.Action => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.Action), searchCriteria, extractor)
        case ReferenceType.Requirement => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.Requirement), searchCriteria, extractor)
        case ReferenceType.Import => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.Import), searchCriteria, extractor)
        case ReferenceType.Type => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.Type), searchCriteria, extractor)
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

  private def extractActions(filePath: String): Set[DocReference] =
    extract(filePath, (l: String, _: String) => filterReferenceTypes(l, ReferenceType.Action), transformReference)

  private def extractProperties(filePath: String): Set[DocReference] =
    extract(filePath, (l: String, _: String) => filterReferenceTypes(l, ReferenceType.Requirement), transformReference)

  private def extractImports(filePath: String): Set[DocReference] =
    extract(filePath, (l: String, _: String) => filterReferenceTypes(l, ReferenceType.Import), transformReference)

  private def extractTypes(filePath: String): Set[DocReference] =
    extract(filePath, (l: String, _: String) => filterReferenceTypes(l, ReferenceType.Type), transformReference)

  def referenceText(name: String, typeString: String): String = {
    val sanitizedName = latexFormatter.sanitizeLine(name)
    s"${typeString}_$sanitizedName"
  }

  def extractTypeDependency(str: String, referenceType: ReferenceType): String = {
    val cleanLine = removeCryptolMetaData(str)
    val name = referenceType match
      case ReferenceType.Requirement => cleanLine.replace("property", "").strip().split(" ").head
      case ReferenceType.Action => cleanLine.split(":").head
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
    val lowerCaseLine = removeCryptolMetaData(line).toLowerCase.strip()
    if lowerCaseLine.startsWith("property") then
      Some(ReferenceType.Requirement)
    else if lowerCaseLine.startsWith("import") then
      Some(ReferenceType.Import)
    else if lowerCaseLine.startsWith("type") then
      Some(ReferenceType.Type)
    else if lowerCaseLine.startsWith("item") then
      Some(ReferenceType.Item)
    else if lowerCaseLine.contains("->") && lowerCaseLine.contains(":") && !lowerCaseLine.contains("::")
    then
      Some(ReferenceType.Action)
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
