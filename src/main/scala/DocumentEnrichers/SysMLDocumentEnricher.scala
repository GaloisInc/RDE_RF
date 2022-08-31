package DocumentEnrichers

import Types.DocumentInfos.{DocumentInfo, SysMLDocumentInfo}
import Types.{DocReference, DocumentType, FileType, ReferenceKeyWords, ReferenceName, ReferenceType}

import java.util.Locale
import scala.util.matching.Regex

class SysMLDocumentEnricher extends DocumentEnricher {
  val keyWordsToRemove: Array[String] = Array("private", "abstract", "id", "def", ";", "\\{")

  //SysML
  val keyWordsToReference: ReferenceKeyWords = ReferenceKeyWords(
    System = "package",
    // Lando SubSystem and SysML Part
    SubSystem = "part",
    // Lando SubSystem and SysML Item
    Component = "item",
    // SysML Use case and Lando Scenario
    Scenario = "use case",
    // SysML and Lando Requirement and Cryptol Properties
    Requirement = "requirement",
    // Lando Event, SysML Action and Cryptol Functions
    Event = "action",
    Connection = "connection",
    Import = "import",
    View = "view",
    ViewPoint = "viewpoint",
    Attribute = "attribute",
  )

  def extractDocumentInfo(filePath: String): SysMLDocumentInfo = {
    require(filePath.nonEmpty)
    require(fileUtil.getFileType(filePath) == "sysml")

    val fileName = fileUtil.getFileName(filePath)
    val packages: Set[DocReference] = extractReferences(filePath, ReferenceType.System)
    val parts: Set[DocReference] = extractReferences(filePath, ReferenceType.SubSystem)
    val items: Set[DocReference] = extractReferences(filePath, ReferenceType.Component)

    val connections: Set[DocReference] = extractReferences(filePath, ReferenceType.Connection)
    val usecases: Set[DocReference] = extractReferences(filePath, ReferenceType.Scenario)
    val requirements: Set[DocReference] = extractReferences(filePath, ReferenceType.Requirement)
    val actions: Set[DocReference] = extractReferences(filePath, ReferenceType.Event)
    val imports: Set[DocReference] = Set.empty[DocReference]
    val views: Set[DocReference] = extractReferences(filePath, ReferenceType.View)
    val viewPoints: Set[DocReference] = extractReferences(filePath, ReferenceType.ViewPoint)
    val attributes: Set[DocReference] = extractReferences(filePath, ReferenceType.Attribute)
    //val ports: Set[DocReference] = extractAttributes(filePath)

    SysMLDocumentInfo(
      fileName,
      filePath,
      packages,
      parts,
      connections,
      usecases,
      requirements,
      actions,
      imports,
      views,
      items,
      //attributes
    )
  }


  def formatLine(line: String, documentInfo: DocumentInfo): String = {
    val references = documentInfo.getAllReferences

    getReferenceType(line) match
      case Some(value) => value match
        case ReferenceType.Event => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.Event))
        case ReferenceType.System => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.System))
        case ReferenceType.SubSystem => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.SubSystem))
        case ReferenceType.Component => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.Component))
        case ReferenceType.Scenario => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.Scenario))
        case ReferenceType.Requirement => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.Requirement))
        case ReferenceType.Connection => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.Connection))
        case ReferenceType.Import => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.Import))
        case ReferenceType.View => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.View))
        case ReferenceType.ViewPoint => line
        case _ => line
      case None => line
  }

  def transformReference(line: String, fileName: String, fileType: FileType): DocReference = {
    val cleanedString = removeKeyWords(line)
    val referenceType = getReferenceType(cleanedString).get
    //todo add type
    val nameAcronym = extractTypeDependency(line, referenceType)
    val reference = referenceText(nameAcronym.name, s"sysml_${fileName}_${referenceType.toString}")
    val referenceInfo = ReferenceName(nameAcronym.name, reference, nameAcronym.acronym)

    DocReference(
      fileName,
      referenceInfo,
      referenceType,
      DocumentType.SysML,
      line,
      Some(latexFormatter.enrichLineWithLabel(line, reference))
    )
  }

  private def extractReferences(filePath: String, referenceType: ReferenceType): Set[DocReference] = {
    extract(filePath, (line: String, _: String) => filterReferenceTypes(line, referenceType), transformReference)
  } ensuring ((refs: Set[DocReference]) => refs.forall(_.referenceType == referenceType))


  def extractTypeDependency(str: String, referenceType: ReferenceType): NameAcronym = {
    val cleanLine = removeAllKeyWordsFromName(str, referenceType)
    if cleanLine.contains(":>")
    then
      val relationTo = cleanLine.split(":>").last
      val name = cleanLine.split(":>").head
      //extractNameAcronym(relationTo, referenceType)
      extractNameAcronym(name, referenceType)
    else if cleanLine.contains(":")
    then
      val typeOf = cleanLine.split(":").last
      val name = cleanLine.split(":").head
      //extractNameAcronym(typeOf, referenceType)
      extractNameAcronym(name, referenceType)
    else
      extractNameAcronym(cleanLine, referenceType)
  }

  def extractNameAcronym(str: String, referenceType: ReferenceType): NameAcronym = {
    val strippedLine = removeAllKeyWordsFromName(str, referenceType)
    val nameRegex = new Regex("'.*'")
    val referenceName = nameRegex findFirstIn strippedLine
    if referenceName.isDefined then
      val highlightedName = referenceName.get
      val name = highlightedName.replaceAll("'", "")
      val acronym = if strippedLine.length > referenceName.get.length then Some(strippedLine.replace(highlightedName, "").strip()) else None
      NameAcronym(name, acronym)
    else
      NameAcronym(strippedLine, None)
  }

  final case class NameAcronym(
                                name: String,
                                acronym: Option[String]
                              )

  def removeAllKeyWordsFromName(name: String, referenceType: ReferenceType): String = {
    val nameToRemove = referenceType match
      case ReferenceType.Component => "item"
      case ReferenceType.SubSystem => "part"
      case ReferenceType.System => "package"
      case ReferenceType.Scenario => "use case"
      case ReferenceType.Requirement => "requirement"
      case ReferenceType.Event => "action"
      case ReferenceType.Connection => "connection"
      case ReferenceType.Import => "impprt"
      case ReferenceType.View => "view"
      case ReferenceType.ViewPoint => "viewpoint"
      case ReferenceType.Type => ""
      case ReferenceType.Attribute => "attribute"

    val cleanedString = name.toLowerCase(Locale.US)
      .replaceFirst(nameToRemove, "")
      .strip()

    removeKeyWords(cleanedString)
  }

  private def filterReferenceTypes(line: String, referenceType: ReferenceType): Boolean = {
    val strippedLine = line.strip()
    strippedLine.nonEmpty
      && !strippedLine.startsWith("//")
      && !strippedLine.startsWith("/*")
      && getReferenceType(strippedLine).nonEmpty
      && getReferenceType(strippedLine).get == referenceType
  }

  def getFileType(path: String): FileType = {
    if (fileUtil.isFileType(path, "action")) FileType.EventFile
    else if (fileUtil.isFileType(path, "requirement")) FileType.RequirementFile
    else if (fileUtil.isFileType(path, "use case")) FileType.ScenarioFile
    else if (fileUtil.isFileType(path, "view")) FileType.ViewFile
    FileType.ComponentFile
  }
}
