package DocumentEnrichers

import Formatter.{LatexFormatter, LatexSanitizer}
import Types.*
import Types.DocumentInfos.{DocumentInfo, LandoDocumentInfo}
import Types.FileType.*

import java.util.Locale
import scala.util.matching.Regex

class LandoDocumentEnricher(override val formatterType: LatexFormatter,
                            override val skipTodos: Boolean = true) extends DocumentEnricher {

  val keyWordsToRemove: Array[String] = Array("private", "requirements", "events", "scenarios")

  //Lando
  val keyWordsToReference: ReferenceKeyWords = ReferenceKeyWords(
    System = "system",
    // Lando SubSystem and SysML Part
    SubSystem = "subsystem",
    // Lando Component and SysML Item
    Component = "component",
    // SysML Use case and Lando Scenario
    //Scenario = "",
    // SysML and Lando Requirement and Cryptol Properties
    //Requirement = "",
    // Lando Event, SysML Action and Cryptol Functions
    //Event = "",
    Import = "import",
  )

  def extractDocumentInfo(filePath: String): LandoDocumentInfo = {
    require(filePath.nonEmpty, "filePath must not be empty")
    require(fileUtil.getFileType(filePath) == "lando", "filePath must be a lando file")
    val references: Set[DocReference] = extractReferences(filePath, FileType.ComponentFile)
    val relations: Set[DocRelation] = extractRelations(filePath)
    val requirements: Set[DocReference] = extractReferences(filePath, FileType.RequirementFile)
    val scenarios: Set[DocReference] = extractReferences(filePath, FileType.ScenarioFile)
    val events: Set[DocReference] = extractReferences(filePath, FileType.EventFile)

    val fileName = fileUtil.getFileName(filePath)
    val enrichedRelations = enrichRelations(relations, references, fileName)

    LandoDocumentInfo(fileName, filePath, references, enrichedRelations, events, requirements, scenarios)
  } ensuring ((landoDoc: DocumentInfo) =>
    landoDoc.documentType == DocumentType.Lando
      && landoDoc.filePath == filePath)

  override def formatLine(line: String, documentInfo: DocumentInfo): String = {
    val references = documentInfo.getAllReferences
    val referenceTypesOfComponent = Set(ReferenceType.Component, ReferenceType.System, ReferenceType.SubSystem)
    getLineType(line, documentInfo.filePath) match
      case LandoLineType.EmptyLine => line
      case LandoLineType.Comment => line
      case LandoLineType.LineToBeSkipped => ""
      case LandoLineType.Relation => extractEnrichedText(line, documentInfo.getRelations)
      case LandoLineType.Requirement => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.Requirement))
      case LandoLineType.Event => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.Event))
      case LandoLineType.Scenario => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.Scenario))
      case LandoLineType.Reference => extractEnrichedText(line, references.filter(ref => referenceTypesOfComponent.contains(ref.getReferenceType)))
  }

  private def enrichRelation(relation: DocRelation, references: Set[DocReference], docName: String): DocRelation = {
    require(references.nonEmpty, "references must not be empty")
    val sourceReference = references.filter(ref => referenceNameMatches(relation.getSourceName, ref.getReference))
    val targetReference = references.filter(ref => referenceNameMatches(relation.getTargetName, ref.getReference))

    //assert(sourceReference.nonEmpty, s"Relation source reference not found: ${relation.getSourceName} in $docName")
    //assert(targetReference.nonEmpty, s"Relation target reference not found: ${relation.getTargetName} in $docName")

    DocRelation(
      relation.getDocumentName,
      relation.getRelationReference,
      relation.getRelationType,
      relation.getOriginalLine,
      sourceReference.headOption,
      targetReference.headOption
    )
  }


  private def getLineType(line: String, documentPath: String): LandoLineType = {
    val lowerLine = line.toLowerCase(Locale.US).strip()
    if (lowerLine.isEmpty) return LandoLineType.EmptyLine
    if (lowerLine.startsWith("//")) return LandoLineType.Comment
    if (lowerLine.startsWith("relation")) return LandoLineType.Relation
    if (lowerLine.startsWith("component") || lowerLine.startsWith("subsystem") || lowerLine.startsWith("system")) return LandoLineType.Reference
    if (line.startsWith("@todo")) return LandoLineType.LineToBeSkipped
    getFileType(documentPath) match
      case FileType.RequirementFile => LandoLineType.Requirement
      case FileType.ScenarioFile => LandoLineType.Scenario
      case FileType.EventFile => LandoLineType.Event
      case FileType.ComponentFile => LandoLineType.Reference
  }

  private def transformReference(line: String, fileName: String, fileType: FileType): DocReference = {
    val referenceOption = getReferenceTypeBasedOnFileType(line, fileType)
    val getReferenceType = referenceOption.get
    val referenceName = extractReferenceName(line, getReferenceType, fileName)
    DocReference(
      fileName, referenceName, getReferenceType, DocumentType.Lando, line
    )
  }

  private def extractReferences(filePath: String, fileType: FileType): Set[DocReference] = {
    if (getFileType(filePath) != fileType) return Set.empty
    val fileChecker = (l: String, p: String) => isOfType(fileType, l, p)
    extract(filePath, fileChecker, transformReference)
  }

  private def enrichRelations(relations: Set[DocRelation], references: Set[DocReference], docName: String): Set[DocRelation] = {
    val enrichedRels = relations.map(rel => enrichRelation(rel, references, docName))
    enrichedRels
  }

  private def isRelation(line: String, prev: String): Boolean = {
    (line.nonEmpty && !line.startsWith("//")) && getRelationType(line).nonEmpty
  }

  private def isOfType(fileType: FileType, line: String, previousLine: String): Boolean = {
    line.nonEmpty && !line.startsWith("//")
      && (fileType == FileType.ComponentFile
      || (fileType != FileType.ComponentFile && (previousLine.isEmpty || previousLine.startsWith("//"))))
      && getReferenceTypeBasedOnFileType(line, fileType).nonEmpty
  }

  private def extractReferenceName(line: String, getReferenceType: ReferenceType, documentName: String): ReferenceName = {
    val strippedLine = line.replace(getReferenceType.toString.toLowerCase(Locale.US), "").strip()

    val acronym = extractAcronym(strippedLine)
    val name = extractReferenceText(strippedLine, acronym, getReferenceType)
    ReferenceName(name, acronym)
  }

  private def extractReferenceText(strippedLine: String, acronym: Option[String], getReferenceType: ReferenceType) = {
    val line = strippedLine.split(" ")
      .dropRight(if acronym.nonEmpty then 1 else 0)
      .mkString(" ").strip()

    if getReferenceType == ReferenceType.Scenario && line.exists(_.isDigit) then
      line.dropWhile(!_.isDigit)
    else line
  }

  private def extractAcronym(strippedLine: String): Option[String] = {
    val acronymRegex = new Regex("\\((.*)\\)")
    val referenceAcronym = acronymRegex findFirstIn strippedLine
    referenceAcronym match
      case Some(value) =>
        Some(value.replace("(", "").replace(")", ""))
      case None => None
  }

  private def extractRelationName(line: String, relationType: RelationType): RelationReference = {
    val strippedString = line.replace("relation", "")
    val sourceTargetStrings = strippedString.split(relationType.toString)
    val source = sourceTargetStrings.head.strip()
    val target = sourceTargetStrings.last.strip()
    RelationReference(source, target)
  }


  private def getReferenceTypeBasedOnFileType(line: String, fileType: FileType): Option[ReferenceType] = {
    fileType match
      case FileType.RequirementFile => if !line.startsWith("requirements") then Some(ReferenceType.Requirement) else None
      case FileType.ScenarioFile => if !line.startsWith("scenarios") then Some(ReferenceType.Scenario) else None
      case FileType.EventFile => if !line.startsWith("events") then Some(ReferenceType.Event) else None
      case _ => getReferenceType(line)
  }

  // Enhance this
  private def getRelationType(line: String): Option[RelationType] = {
    val lowerCaseLine = line.toLowerCase(Locale.US)
    if lowerCaseLine.startsWith("relation") then
      if lowerCaseLine.contains("client") then
        Some(RelationType.client)
      else if lowerCaseLine.contains("inherit") then
        Some(RelationType.inherit)
      else if lowerCaseLine.contains("contains") then
        Some(RelationType.contains)
      else
        None
    else
      None
  }

  def extractRelations(filePath: String): Set[DocRelation] = {
    def transformRelation(line: String, fileName: String, fileType: FileType): DocRelation = {
      val relationType = getRelationType(line).get
      val relationReference = extractRelationName(line, relationType)
      DocRelation(
        fileName,
        relationReference,
        relationType,
        line,
        None,
        None,
      )
    }

    extract(filePath, isRelation, transformRelation)
  }

  def getFileType(path: String): FileType = {
    if (fileUtil.isFileType(path, "events")) return FileType.EventFile
    if (fileUtil.isFileType(path, "requirements")) return FileType.RequirementFile
    if (fileUtil.isFileType(path, "scenarios")) return FileType.ScenarioFile
    FileType.ComponentFile
  }


}


