package DocumentEnrichers

import Formatter.LatexFormatter
import Specs.FileSpecs
import Types.DocReference.DocReference
import Types.DocumentInfos.SysMLDocumentInfo
import Types.Reference.{Ref, RefinementRef, TypeRef}
import Types.{DocumentType, ReferenceName, ReferenceType}
import Utils.{Control, FileUtil}

import scala.util.matching.Regex

class SysMLDocumentEnricher(override val formatterType: LatexFormatter,
                            override val skipTodos: Boolean = true) extends DocumentEnricher[SysMLDocumentInfo](formatterType, skipTodos) {

  private def composeRegex(list: List[String]): Regex = {
    list.mkString("\\s*").r
  }

  private def capture(regex: String): String = {
    s"($regex)"
  }

  private def ignoreGroup(regex: String): String = {
    s"(?:$regex)"
  }

  private def optional(regex: String): String = {
    s"(?:$regex)?"
  }

  private def nonCapturing(regex: String): String = {
    s"(?:$regex)"
  }

  private def choice(list: List[String]): String = {
    list.mkString("|")
  }


  private val abstractRegex: String = optional("abstract")
  private val defRegex: String = optional("def")
  private val idRegex: String = optional("id")
  private val wordRegex: String = optional("<(\\w*)>")
  private val nameRegex = nonCapturing(choice(List("(\\w+)", "'(.*?)'")))
  private val arrayRegex = s"$nameRegex\\s*\\[\\s*(\\d+)\\s*\\]"
  private val refinement: String = optional("(:>|:)\\s*(.*?)")
  private val semicolon: String = optional(";+")

  val systemRegex: Regex = composeRegex(List("^package", idRegex, wordRegex, nameRegex, semicolon))
  val componentRegex: Regex = composeRegex(List(abstractRegex, "item", defRegex, idRegex, wordRegex, nameRegex, refinement, semicolon))
  val subsystemRegex: Regex = composeRegex(List(abstractRegex, "part", defRegex, idRegex, wordRegex, nameRegex, refinement, semicolon))
  val attributeRegex: Regex = composeRegex(List("^attribute", defRegex, wordRegex, nameRegex, refinement, semicolon))
  val requirementRegex: Regex = composeRegex(List("^requirement", defRegex, idRegex, wordRegex, nameRegex, refinement, semicolon))
  val usecaseRegex: Regex = composeRegex(List("^use case", defRegex, idRegex, wordRegex, nameRegex, refinement, semicolon))
  val actionRegex: Regex = composeRegex(List("^action", defRegex, idRegex, wordRegex, nameRegex, refinement, semicolon))
  val viewRegex: Regex = composeRegex(List("^view", defRegex, idRegex, nameRegex, refinement, semicolon))
  val viewPointRegex: Regex = composeRegex(List("^viewpoint", defRegex, idRegex, wordRegex, nameRegex, semicolon))

  val importRegex: Regex = """^import\\s*('(.*?)')::.*""".r
  val connectionRegex: Regex = List("^connect", "(.*?)", "=to", "(.*?)").mkString("\\s*").r

  def parseDocument(filePath: String): SysMLDocumentInfo = {
    require(FileSpecs.fileChecks(Set(filePath), Set("sysml")), "filePath must be a sysml file")
    logger.info(s"Parsing file $filePath")

    val fileName = FileUtil.fileNameFromPath(filePath)
    val references = Control.extractReferences(filePath, (l: String) => transformReference(l, fileName))

    val packages: Set[DocReference] = references.filter(_.getReferenceType == ReferenceType.System)
    val parts: Set[DocReference] = references.filter(_.getReferenceType == ReferenceType.SubSystem)
    val items: Set[DocReference] = references.filter(_.getReferenceType == ReferenceType.Component)
    val attributes: Set[DocReference] = Set.empty[DocReference] // references.filter(_.getReferenceType == ReferenceType.Attribute)
    val requirements: Set[DocReference] = references.filter(_.getReferenceType == ReferenceType.Requirement)
    val usecases: Set[DocReference] = references.filter(_.getReferenceType == ReferenceType.Scenario)
    val actions: Set[DocReference] = references.filter(_.getReferenceType == ReferenceType.Event)
    val views: Set[DocReference] = references.filter(_.getReferenceType == ReferenceType.View)
    val viewPoints: Set[DocReference] = references.filter(_.getReferenceType == ReferenceType.ViewPoint)
    val connections: Set[DocReference] = references.filter(_.getReferenceType == ReferenceType.Connection)

    //val connections: Set[DocReference] = extractReferences(filePath, ReferenceType.Connection)
    val imports: Set[DocReference] = Set.empty[DocReference]
    //val ports: Set[DocReference] = extractReferences(filePath, ReferenceType.Port)

    new SysMLDocumentInfo(
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
      attributes
    )
  }


  def formatLine(line: String, documentInfo: SysMLDocumentInfo): String = {
    val references = documentInfo.getAllReferences
    cleanString(line) match {
      case systemRegex(_, _) => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.System))
      case componentRegex(_, _, _, _) => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.Component))
      case attributeRegex(_, _, _, _, _) => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.Attribute))
      case requirementRegex(_, _, _, _, _) => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.Requirement))
      case usecaseRegex(_, _, _, _) => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.Scenario))
      case actionRegex(_, _, _, _) => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.Event))
      case importRegex(_) => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.Import))
      case viewRegex(_, _) => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.View))
      case viewPointRegex(_) => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.ViewPoint))
      case subsystemRegex(_, _, _, _) => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.SubSystem))
      case _ => line
    }
  }

  private def cleanString(line: String): String = {
    line.trim().stripSuffix(";;").stripSuffix(";").stripSuffix("}").stripSuffix("{").trim()
  }

  def transformReference(line: String, fileName: String): Option[DocReference] = {
    def emptyIfNull(s: String): String = {
      logger.debug(s"emptyIfNull: $s")
      if (s == null) "" else s
    }

    def noneIfNull(s: String): Option[String] = Option(s)

    def createRef(symbol: String, name: String): Ref = {
      require(symbol.nonEmpty, "symbol cannot be empty")
      require(name.nonEmpty, "ref cannot be empty")
      require(symbol == ":" || symbol == ":>", "symbol must be : or :>")
      if (symbol == ":>") RefinementRef(name, symbol)
      else TypeRef(name, symbol)
    }

    def refinementRefs(symbol: String, referenceString: String): Option[Set[Ref]] = {
      if (symbol == null || symbol.isEmpty || referenceString == null || referenceString.isEmpty) None
      else Some(referenceString.split(",").map(_.trim).map(createRef(symbol.trim(), _)).toSet)
    }

    def findNonEmptyString(option1: String, option2: String): String = {
      if (option1 != null && option1.nonEmpty) option1
      else if (option2 != null && option2.nonEmpty) option2
      else ""
    }

    val extractedReference = cleanString(line) match {
      case systemRegex(acronym, name, nameQuoted) => Some(new DocReference(fileName, ReferenceName(findNonEmptyString(name, nameQuoted), noneIfNull(acronym)), ReferenceType.System, DocumentType.SysML, line))
      case componentRegex(acronym, name, nameQuoted, symbol, references) => Some(new DocReference(fileName, ReferenceName(findNonEmptyString(name, nameQuoted), noneIfNull(acronym)), ReferenceType.Component, DocumentType.SysML, line, references = refinementRefs(symbol, references)))
      case subsystemRegex(acronym, name, nameQuoted, symbol, references) => Some(new DocReference(fileName, ReferenceName(findNonEmptyString(name, nameQuoted), noneIfNull(acronym)), ReferenceType.SubSystem, DocumentType.SysML, line, references = refinementRefs(symbol, references)))
      case attributeRegex(acronym, name, nameQuoted, symbol, references) => Some(new DocReference(fileName, ReferenceName(findNonEmptyString(name, nameQuoted), noneIfNull(acronym)), ReferenceType.Attribute, DocumentType.SysML, line, references = refinementRefs(symbol, references)))
      case requirementRegex(acronym, name, nameQuoted, symbol, references) => Some(new DocReference(fileName, ReferenceName(findNonEmptyString(name, nameQuoted), noneIfNull(acronym)), ReferenceType.Requirement, DocumentType.SysML, line, references = refinementRefs(symbol, references)))
      case actionRegex(acronym, name, nameQuoted, symbol, references) => Some(new DocReference(fileName, ReferenceName(findNonEmptyString(name, nameQuoted), noneIfNull(acronym)), ReferenceType.Event, DocumentType.SysML, line, references = refinementRefs(symbol, references)))
      case importRegex(name) => Some(new DocReference(fileName, ReferenceName(emptyIfNull(name), None), ReferenceType.Import, DocumentType.SysML, line))
      case viewRegex(name, nameQuoted, symbol, references) => Some(new DocReference(fileName, ReferenceName(findNonEmptyString(name, nameQuoted), None), ReferenceType.View, DocumentType.SysML, line, references = refinementRefs(symbol, references)))
      case viewPointRegex(name) => Some(new DocReference(fileName, ReferenceName(emptyIfNull(name), None), ReferenceType.ViewPoint, DocumentType.SysML, line))
      case usecaseRegex(acronym, name, nameQuoted, symbol, references) => Some(new DocReference(fileName, ReferenceName(findNonEmptyString(name, nameQuoted), noneIfNull(acronym)), ReferenceType.Scenario, DocumentType.SysML, line, references = refinementRefs(symbol, references)))
      //case connectionRegex(source, target) => DocRelation(fileName, ReferenceName(name, noneIfNull(acronym)), ReferenceType.Connection, DocumentType.SysML, line,)
      case _ => None
    }

    extractedReference
  }

}
