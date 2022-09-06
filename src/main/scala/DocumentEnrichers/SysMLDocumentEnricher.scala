package DocumentEnrichers

import Formatter.LatexFormatter
import Types.DocumentInfos.{DocumentInfo, SysMLDocumentInfo}
import Types.{DocReference, DocumentType, FileType, ReferenceKeyWords, ReferenceName, ReferenceType}

import java.util.Locale
import scala.util.matching.Regex

class SysMLDocumentEnricher(override val formatterType: LatexFormatter,
                            override val skipTodos: Boolean = true) extends DocumentEnricher {
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
        case ReferenceType.Event => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.Event))
        case ReferenceType.System => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.System))
        case ReferenceType.SubSystem => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.SubSystem))
        case ReferenceType.Component => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.Component))
        case ReferenceType.Scenario => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.Scenario))
        case ReferenceType.Requirement => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.Requirement))
        case ReferenceType.Connection => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.Connection))
        case ReferenceType.Import => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.Import))
        case ReferenceType.View => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.View))
        case ReferenceType.ViewPoint => line
        case _ => line
      case None => line
  }

  def transformReference(line: String, fileName: String, fileType: FileType): DocReference = {
    val cleanedString = removeKeyWords(line)
    val referenceType = getReferenceType(cleanedString).get
    //todo add type
    val nameAcronym = extractTypeDependency(line, referenceType)
    val referenceInfo = ReferenceName(nameAcronym.name, nameAcronym.acronym)

    DocReference(
      fileName,
      referenceInfo,
      referenceType,
      DocumentType.SysML,
      line
    )
  }

  private def extractReferences(filePath: String, getReferenceType: ReferenceType): Set[DocReference] = {
    extract(filePath, (line: String, _: String) => filterReferenceTypes(line, getReferenceType), transformReference)
  } ensuring ((refs: Set[DocReference]) => refs.forall(_.getReferenceType == getReferenceType))


  def extractTypeDependency(str: String, getReferenceType: ReferenceType): NameAcronym = {
    val cleanLine = removeAllKeyWordsFromName(str, getReferenceType)
    if cleanLine.contains(":>")
    then
      val relationTo = cleanLine.split(":>").last
      val name = cleanLine.split(":>").head
      //extractNameAcronym(relationTo, getReferenceType)
      extractNameAcronym(name, getReferenceType)
    else if cleanLine.contains(":")
    then
      val typeOf = cleanLine.split(":").last
      val name = cleanLine.split(":").head
      //extractNameAcronym(typeOf, getReferenceType)
      extractNameAcronym(name, getReferenceType)
    else
      extractNameAcronym(cleanLine, getReferenceType)
  }

  def extractNameAcronym(str: String, getReferenceType: ReferenceType): NameAcronym = {
    val strippedLine = removeAllKeyWordsFromName(str, getReferenceType)
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

  def removeAllKeyWordsFromName(name: String, getReferenceType: ReferenceType): String = {
    val nameToRemove = getReferenceType match
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
