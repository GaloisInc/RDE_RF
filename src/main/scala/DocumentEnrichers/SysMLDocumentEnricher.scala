package DocumentEnrichers

import Formatter.LatexFormatter
import Types.DocReference.DocReference
import Types.DocumentInfos.{DocumentInfo, SysMLDocumentInfo}
import Types.Reference.{Ref, RefinementRef, TypeRef}
import Types.{DocumentType, ReferenceName, ReferenceType}
import Utils.{Control, FileUtil}

import scala.util.matching.Regex

class SysMLDocumentEnricher(override val formatterType: LatexFormatter,
                            override val skipTodos: Boolean = true) extends DocumentEnricher(formatterType, skipTodos) {

  val systemRegex: Regex = """^(?:package)\s*(?:def)?\s*(?:id)?\s*(\w*)?\s*(?:'(.*?)')?""".r
  val componentRegex: Regex = """^(?:abstract)?\s*(?:item)\s*(?:def)?\s*(?:id)?\s*(\w*)?\s*(?:'(.*?)')?(?:\s*(:>|:)\s*(.*))?""".r
  val subsystemRegex: Regex = """^(?:abstract)?\s*part\s*(?:def)?\s*(?:id)?\s*(\w*)?\s*(?:'(.*?)')?(?:\s*(:>|:)\s*(.*))?""".r
  val attributeRegex: Regex = """^(?:abstract)?\s*attribute\s*(?:def)?\s*(?:id)?\s*(?=.)\s*(\w*)?\s*(?:'(.*?)')?(?:\s*(:>|:)\s*(.*))?""".r
  val requirementRegex: Regex = """^requirement\s*(?:def)?\s*(?:id)?\s*(\w*)?\s*(?:'(.*?)')?\s*(?:(:>|:)?\s*(.*))?""".r
  val usecaseRegex: Regex = """^use case\s*(?:def)?\s*(?:id)?\s*(\w*)?\s*(?:'(.*?)')?\s*(?:(:>|:)?\s*(.*))?""".r
  val actionRegex: Regex = """^action\s*(?:def)?\s*(?:id)?\s*(\w*)?\s*(?:'(.*?)')?\s*(:>|:)?\s*(.*)""".r
  val importRegex: Regex = """^import\\s*('(.*?)')::.*""".r
  val viewRegex: Regex = """^view\s*(?:def)?\s*(?:id)?\s*(?:'(.*?)')(?:\s*:\s*(?:'(.*)'))?""".r
  val viewPointRegex: Regex = """^viewpoint\s*(?:def)?\s*(?:id)?\s*'(.*?)'""".r
  val connectionRegex: Regex = """^connect\s*(\.*)\s=to\s+(.*?)""".r

  def parseDocument(filePath: String): SysMLDocumentInfo = {
    require(filePath.nonEmpty)
    require(FileUtil.getFileType(filePath) == "sysml")

    val fileName = FileUtil.getFileName(filePath)
    val references = Control.extractReferences(filePath, (l: String) => transformReference(l, fileName))

    val packages: Set[DocReference] = references.filter(_.getReferenceType == ReferenceType.System)
    val parts: Set[DocReference] = references.filter(_.getReferenceType == ReferenceType.SubSystem)
    val items: Set[DocReference] = references.filter(_.getReferenceType == ReferenceType.Component)
    val attributes: Set[DocReference] = Set.empty // references.filter(_.getReferenceType == ReferenceType.Attribute)
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


  def formatLine(line: String, documentInfo: DocumentInfo): String = {
    val references = documentInfo.getAllReferences
    cleanString(line) match {
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
  }

  private def cleanString(line: String): String = {
    line.trim().stripSuffix(";;").stripSuffix(";").stripSuffix("}").stripSuffix("{").trim()
  }

  def transformReference(line: String, fileName: String): Option[DocReference] = {
    def emptyIfNull(s: String): String = if (s == null) "" else s

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
      else
        Some(referenceString.split(",").map(_.trim).map(createRef(symbol.trim(), _)).toSet)
    }

    val extractedReference = cleanString(line) match {
      case systemRegex(acronym, name) => Some(new DocReference(fileName, ReferenceName(emptyIfNull(name), noneIfNull(acronym)), ReferenceType.System, DocumentType.SysML, line))
      case componentRegex(acronym, name, symbol, references) => Some(new DocReference(fileName, ReferenceName(emptyIfNull(name), noneIfNull(acronym)), ReferenceType.Component, DocumentType.SysML, line, references = refinementRefs(symbol, references)))
      case subsystemRegex(acronym, name, symbol, references) => Some(new DocReference(fileName, ReferenceName(emptyIfNull(name), noneIfNull(acronym)), ReferenceType.SubSystem, DocumentType.SysML, line, references = refinementRefs(symbol, references)))
      case attributeRegex(acronym, name, symbol, references) => Some(new DocReference(fileName, ReferenceName(emptyIfNull(name), noneIfNull(acronym)), ReferenceType.Attribute, DocumentType.SysML, line, references = refinementRefs(symbol, references)))
      case requirementRegex(acronym, name, symbol, references) => Some(new DocReference(fileName, ReferenceName(emptyIfNull(name), noneIfNull(acronym)), ReferenceType.Requirement, DocumentType.SysML, line, references = refinementRefs(symbol, references)))
      case actionRegex(acronym, name, symbol, references) => Some(new DocReference(fileName, ReferenceName(emptyIfNull(name), noneIfNull(acronym)), ReferenceType.Event, DocumentType.SysML, line, references = refinementRefs(symbol, references)))
      case importRegex(name) => Some(new DocReference(fileName, ReferenceName(emptyIfNull(name), None), ReferenceType.Import, DocumentType.SysML, line))
      case viewRegex(name, references) => Some(new DocReference(fileName, ReferenceName(emptyIfNull(name), None), ReferenceType.View, DocumentType.SysML, line, references = refinementRefs(":", references)))
      case viewPointRegex(name) => Some(new DocReference(fileName, ReferenceName(emptyIfNull(name), None), ReferenceType.ViewPoint, DocumentType.SysML, line))
      case usecaseRegex(acronym, name, symbol, references) => Some(new DocReference(fileName, ReferenceName(emptyIfNull(name), noneIfNull(acronym)), ReferenceType.Scenario, DocumentType.SysML, line, references = refinementRefs(symbol, references)))
      //case connectionRegex(source, target) => DocRelation(fileName, ReferenceName(name, noneIfNull(acronym)), ReferenceType.Connection, DocumentType.SysML, line,)
      case _ => None
    }

    extractedReference
  }

}
