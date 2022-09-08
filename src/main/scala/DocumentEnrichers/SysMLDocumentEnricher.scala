package DocumentEnrichers

import Formatter.LatexFormatter
import Types.*
import Types.DocumentInfos.{DocumentInfo, SysMLDocumentInfo}
import Utils.FileUtil

import java.util.Locale
import scala.util.matching.Regex

class SysMLDocumentEnricher(override val formatterType: LatexFormatter,
                            override val skipTodos: Boolean = true) extends DocumentEnricher {
  val keyWordsToRemove: Array[String] = Array("private", "abstract", "id", "def", ";", "\\{")

  val systemRegex: Regex = """^(?:package)\s*(?:def)?\s*(?:id)?\s*(\w*)?\s*(?:'(.*?)')?""".r
  val componentRegex: Regex = """^(?:abstract)?\s*(?:item)\s*(?:def)?\s*(?:id)?\s*(\w*)?\s*(?:'(.*?)')?(?:\s*(:>|:)\s*(.*))?""".r
  val subsystemRegex = """^(?:abstract)?\s*part\s*(?:def)?\s*(?:id)?\s*(\w*)?\s*(?:'(.*?)')?(?:\s*(:>|:)\s*(.*))?""".r
  val attributeRegex = """^(?:abstract)?\s*attribute\s*(?:def)?\s*(?:id)?\s*(?=.)\s*(\w*)?\s*(?:'(.*?)')?(?:\s*(:>|:)\s*(.*))?""".r
  val requirementRegex = """^requirement\s*(?:def)?\s*(?:id)?\s*(\w*)?\s*(?:'(.*?)')?\s*(?:(:>|:)?\s*(.*))?""".r
  val usecaseRegex = """^use case\s*(?:def)?\s*(?:id)?\s*(\w*)?\s*(?:'(.*?)')?\s*(?:(:>|:)?\s*(.*))?""".r
  val actionRegex = """^action\s*(?:def)?\s*(?:id)?\s*(\w*)?\s*(?:'(.*?)')?\s*(:>|:)?\s*(.*)""".r
  val importRegex = """^import\s*(?:def)?\s*(?:id)?\s*(\w*)?\s*('(.*?)')?""".r
  val viewRegex = """^view\s*(?:def)?\s*(?:id)?\s*(?:'(.*?)')(?:\s*:\s*(?:'(.*)'))?""".r
  val viewPointRegex = """^viewpoint\s*(?:def)?\s*(?:id)?\s*'(.*?)'""".r
  val connectionRegex = """^connect\s*(\.*)\s*to(.*?)""".r


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
    require(FileUtil.getFileType(filePath) == "sysml")

    val fileName = FileUtil.getFileName(filePath)
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
    cleanString(line) match
      case systemRegex(_, _) => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.System))
      case componentRegex(_, _, _, _) => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.Component))
      case subsystemRegex(_, _, _, _) => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.SubSystem))
      case attributeRegex(_, _, _, _) => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.Attribute))
      case requirementRegex(_, _, _, _) => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.Requirement))
      case usecaseRegex(_, _, _, _) => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.Scenario))
      case actionRegex(_, _, _, _) => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.Event))
      case importRegex(_) => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.Import))
      case viewRegex(_, _) => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.View))
      case viewPointRegex(_) => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.ViewPoint))
      case _ => line
  }

  def cleanString(line: String): String = {
    line.strip().stripSuffix(";;").stripSuffix(";").stripSuffix("}").stripSuffix("{").strip()
  }

  def transformReference(line: String, fileName: String, fileType: FileType): DocReference = {
    def emptyIfNull(s: String): String = if (s == null) "" else s
    def noneIfNull(s: String): Option[String] = if (s == null) None else Some(s)

    val extractedReference: DocReference = cleanString(line) match
      case systemRegex(acronym, name) => DocReference(fileName, ReferenceName(emptyIfNull(name), noneIfNull(acronym)), ReferenceType.System, DocumentType.SysML, line)
      case componentRegex(acronym, name, symbol, references) => DocReference(fileName, ReferenceName(emptyIfNull(name), noneIfNull(acronym)), ReferenceType.Component, DocumentType.SysML, line)
      case subsystemRegex(acronym, name, symbol, references) => DocReference(fileName, ReferenceName(emptyIfNull(name), noneIfNull(acronym)), ReferenceType.SubSystem, DocumentType.SysML, line)
      case attributeRegex(acronym, name, symbol, references) => DocReference(fileName, ReferenceName(emptyIfNull(name), noneIfNull(acronym)), ReferenceType.Attribute, DocumentType.SysML, line)
      case requirementRegex(acronym, name, symbol, references) => DocReference(fileName, ReferenceName(emptyIfNull(name), noneIfNull(acronym)), ReferenceType.Requirement, DocumentType.SysML, line)
      case usecaseRegex(acronym, name, symbol, references) => DocReference(fileName, ReferenceName(emptyIfNull(name), noneIfNull(acronym)), ReferenceType.Scenario, DocumentType.SysML, line)
      case actionRegex(acronym, name, symbol, references) => DocReference(fileName, ReferenceName(emptyIfNull(name), noneIfNull(acronym)), ReferenceType.Event, DocumentType.SysML, line)
      case importRegex(name) => DocReference(fileName, ReferenceName(emptyIfNull(name), None), ReferenceType.Import, DocumentType.SysML, line)
      case viewRegex(name, references) => DocReference(fileName, ReferenceName(emptyIfNull(name), None), ReferenceType.View, DocumentType.SysML, line)
      case viewPointRegex(name) => DocReference(fileName, ReferenceName(emptyIfNull(name), None), ReferenceType.ViewPoint, DocumentType.SysML, line)
      //case connectionRegex(source, target) => DocReference(fileName, ReferenceName(name, noneIfNull(acronym)), ReferenceType.Connection, DocumentType.SysML, line)
      case _ => DocReference(fileName, ReferenceName("", None), ReferenceType.Type, DocumentType.SysML, line)

    extractedReference
  }

  private def extractReferences(filePath: String, getReferenceType: ReferenceType): Set[DocReference] = {
    extract(filePath, (line: String, _: String) => filterReferenceTypes(line, getReferenceType), transformReference)
  } ensuring ((refs: Set[DocReference]) => refs.forall(_.getReferenceType == getReferenceType))

  private def filterReferenceTypes(line: String, referenceType: ReferenceType): Boolean = {
    val cleanedString = cleanString(line)
    val returnValue = referenceType match
      case ReferenceType.System => cleanedString.matches(systemRegex.toString())
      case ReferenceType.Component => cleanedString.matches(componentRegex.toString())
      case ReferenceType.SubSystem => cleanedString.matches(subsystemRegex.toString())
      case ReferenceType.Attribute => cleanedString.matches(attributeRegex.toString())
      case ReferenceType.Requirement => cleanedString.matches(requirementRegex.toString())
      case ReferenceType.Scenario => cleanedString.matches(usecaseRegex.toString())
      case ReferenceType.Event => cleanedString.matches(actionRegex.toString())
      case ReferenceType.Import => cleanedString.matches(importRegex.toString())
      case ReferenceType.View => cleanedString.matches(viewRegex.toString())
      case ReferenceType.ViewPoint => cleanedString.matches(viewPointRegex.toString())
      case ReferenceType.Connection => cleanedString.matches(connectionRegex.toString())
      case _ => false
    returnValue
  }

  def getFileType(path: String): FileType = {
    if (FileUtil.isOfFileType(path, "action")) FileType.EventFile
    else if (FileUtil.isOfFileType(path, "requirement")) FileType.RequirementFile
    else if (FileUtil.isOfFileType(path, "use case")) FileType.ScenarioFile
    else if (FileUtil.isOfFileType(path, "view")) FileType.ViewFile
    FileType.ComponentFile
  }
}
