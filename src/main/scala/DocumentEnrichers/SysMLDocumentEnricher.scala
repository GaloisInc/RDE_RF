package DocumentEnrichers

import Types.{DocReference, DocumentInfo, FileType, ReferenceName, ReferenceType, SysMLDocumentInfo, DocumentType}

import scala.util.matching.Regex

class SysMLDocumentEnricher extends DocumentEnricher {
  // Reads a Document to create an object of the necessary information to enrich the document.
  def extractDocumentInfo(filePath: String): SysMLDocumentInfo = {
    require(filePath.nonEmpty)
    require(fileUtil.getFileType(filePath) == "sysml")

    val fileName = fileUtil.getFileName(filePath)
    val parts: Set[DocReference] = extractParts(filePath)
    val packages: Set[DocReference] = extractPackages(filePath)
    val connections: Set[DocReference] = extractConnection(filePath)
    val usecases: Set[DocReference] = extractUseCases(filePath)
    val requirements: Set[DocReference] = extractRequirements(filePath)
    val actions: Set[DocReference] = extractActions(filePath)
    val imports: Set[DocReference] = Set.empty[DocReference]
    val views: Set[DocReference] = extractViews(filePath)
    val items: Set[DocReference] = extractItems(filePath)
 
    SysMLDocumentInfo(fileName, filePath, packages, parts, connections, usecases, requirements, actions, imports, views, items)
  }


  def formatLine(line: String, documentInfo: DocumentInfo): String = {
    def searchCriteria = (ref: DocReference, srcLine: String) => ref.originalLine == srcLine

    def extractor = (ref: DocReference) => ref.enrichedLine.get

    val references = documentInfo.getAllReferences

    sysmlType(line) match
      case Some(value) => value match
        case ReferenceType.Action => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.Action), searchCriteria, extractor)
        case ReferenceType.Package => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.Package), searchCriteria, extractor)
        case ReferenceType.UseCase => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.UseCase), searchCriteria, extractor)
        case ReferenceType.Part => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.Part), searchCriteria, extractor)
        case ReferenceType.Requirement => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.Requirement), searchCriteria, extractor)
        case ReferenceType.Connection => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.Connection), searchCriteria, extractor)
        case ReferenceType.Import => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.Import), searchCriteria, extractor)
        case ReferenceType.View => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.View), searchCriteria, extractor)
        case ReferenceType.ViewPoint => line
        case ReferenceType.Item => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.Item), searchCriteria, extractor)
        case _ => line
      case None => line
  }

  def transformReference(line: String, fileName: String, fileType: FileType): DocReference = {
    val strippedLine = removeSysMLMetadata(line)
    val sysMlType = sysmlType(strippedLine).get
    //todo add type
    val name = extractTypeDependency(line, sysMlType)
    val reference = referenceText(name, s"sysml_${fileName}_${sysMlType.toString}")
    val referenceInfo = ReferenceName(name, reference)

    DocReference(
      fileName,
      referenceInfo,
      sysMlType,
      DocumentType.SysML,
      line,
      Some(latexFormatter.enrichLineWithLabel(line, reference))
    )
  }

  private def extractParts(filePath: String): Set[DocReference] =
    extract(filePath, (l: String, _: String) => filterReferenceTypes(l, ReferenceType.Part), transformReference)

  private def extractPackages(filePath: String): Set[DocReference] =
    extract(filePath, (l: String, _: String) => filterReferenceTypes(l, ReferenceType.Package), transformReference)

  private def extractItems(filePath: String): Set[DocReference] =
    extract(filePath, (l: String, _: String) => filterReferenceTypes(l, ReferenceType.Item), transformReference)

  private def extractViewPoints(filePath: String): Set[DocReference] =
    extract(filePath, (l: String, _: String) => filterReferenceTypes(l, ReferenceType.ViewPoint), transformReference)

  private def extractViews(filePath: String): Set[DocReference] =
    extract(filePath, (l: String, _: String) => filterReferenceTypes(l, ReferenceType.View), transformReference)

  private def extractRequirements(filePath: String): Set[DocReference] =
    extract(filePath, (l: String, _: String) => filterReferenceTypes(l, ReferenceType.Requirement), transformReference)

  private def extractUseCases(filePath: String): Set[DocReference] =
    extract(filePath, (l: String, _: String) => filterReferenceTypes(l, ReferenceType.UseCase), transformReference)

  private def extractConnection(filePath: String): Set[DocReference] =
    extract(filePath, (l: String, _: String) => filterReferenceTypes(l, ReferenceType.Connection), transformReference)

  private def extractActions(filePath: String): Set[DocReference] =
    extract(filePath, (l: String, _: String) => filterReferenceTypes(l, ReferenceType.Action), transformReference)

  private def extractImports(filePath: String): Set[DocReference] =
    extract(filePath, (l: String, _: String) => filterReferenceTypes(l, ReferenceType.Import), transformReference)

  def referenceText(name: String, typeString: String): String = {
    val sanitizedName = latexFormatter.sanitizeLine(name)
    s"${typeString}_$sanitizedName"
  }

  def extractTypeDependency(str: String, sysMLType: ReferenceType): String = {
    val strippedLine = str.replace(sysMLType.toString.toLowerCase, "").strip()
    val cleanLine = removeSysMLMetadata(strippedLine)
    if cleanLine.contains(":>")
    then
      val relationTo = cleanLine.split(":>").last
      val name = cleanLine.split(":>").head
      extractName(relationTo, sysMLType)
      extractName(name, sysMLType)
    else if cleanLine.contains(":")
    then
      val typeOf = cleanLine.split(":").last
      val name = cleanLine.split(":").head
      extractName(typeOf, sysMLType)
      extractName(name, sysMLType)
    else
      extractName(cleanLine, sysMLType)
  }

  def extractName(str: String, sysMLType: ReferenceType): String = {
    val strippedLine = str.replace(sysMLType.toString.toLowerCase, "").strip()
    val acronymRegex = new Regex("'.*'")
    val referenceName = acronymRegex findFirstIn strippedLine
    if referenceName.isDefined then
      referenceName.get.replaceAll("'", "")
    else
      strippedLine
  }

  // So far only support for lando types
  private def sysmlType(line: String): Option[ReferenceType] = {
    val lowerCaseLine = removeSysMLMetadata(line).toLowerCase.strip()
    if lowerCaseLine.startsWith("action") then
      Some(ReferenceType.Action)
    else if lowerCaseLine.startsWith("package") then
      Some(ReferenceType.Package)
    else if lowerCaseLine.startsWith("use case") then
      Some(ReferenceType.UseCase)
    else if lowerCaseLine.startsWith("requirement") then
      Some(ReferenceType.Requirement)
    else if lowerCaseLine.startsWith("part") then
      Some(ReferenceType.Part)
    else if lowerCaseLine.startsWith("connection") then
      Some(ReferenceType.Connection)
    else if lowerCaseLine.startsWith("import") then
      Some(ReferenceType.Import)
    else if lowerCaseLine.startsWith("view") then
      Some(ReferenceType.View)
    else if lowerCaseLine.startsWith("item") then
      Some(ReferenceType.Item)
    else
      None
  }

  private def filterReferenceTypes(line: String, sysMLType: ReferenceType): Boolean = {
    val strippedLine = line.strip()
    strippedLine.nonEmpty && !strippedLine.startsWith("//") && !strippedLine.startsWith("/*") && sysmlType(strippedLine).nonEmpty && sysmlType(strippedLine).get == sysMLType
  }

  private def removeSysMLMetadata(line: String): String = {
    line.replaceAll("abstract", "")
      .replaceAll("private", "")
      .replaceAll("id", "")
      .replaceAll("def", "")
      .replaceAll(";", "")
      .replaceAll("\\{", "")
      .replaceAll("\\}", "")

      .strip()
  } ensuring ((res: String) => res.length <= line.length)

  def getFileType(path: String): FileType = {
    if (fileUtil.isFileType(path, "action")) return FileType.EventFile
    if (fileUtil.isFileType(path, "requirement")) return FileType.RequirementFile
    if (fileUtil.isFileType(path, "use case")) return FileType.ScenarioFile
    if (fileUtil.isFileType(path, "view")) return FileType.ViewFile
    FileType.ComponentFile
  }
}
