package DocumentEnrichers

import Types.FileType.{ComponentFile, EventFile, RequirementFile, ScenarioFile, ViewFile}
import Types.{DocReference, DocRelation, DocumentInfo, FileType, LandoDocumentInfo, LandoLineType, ReferenceName, ReferenceType, RelationReference, RelationType, DocumentType}

import scala.util.matching.Regex


class LandoDocumentEnricher extends DocumentEnricher {
  def extractDocumentInfo(filePath: String): LandoDocumentInfo = {
    require(filePath.nonEmpty)
    require(fileUtil.getFileType(filePath) == "lando")
    val references: Set[DocReference] = extractReferences(filePath)
    val relations: Set[DocRelation] = extractRelations(filePath)
    val requirements: Set[DocReference] = extractRequirements(filePath)
    val scenarios: Set[DocReference] = extractScenarios(filePath)
    val events: Set[DocReference] = extractEvents(filePath)

    val fileName = fileUtil.getFileName(filePath)
    val enrichedRelations = enrichRelations(relations, references, fileName)

    LandoDocumentInfo(fileName, filePath, references, enrichedRelations, events, requirements, scenarios)
  } ensuring ((landoDoc: DocumentInfo) =>
    landoDoc.documentType == DocumentType.Lando
      && landoDoc.filePath == filePath)

  override def formatLine(line: String, documentInfo: DocumentInfo): String = {
    val references = documentInfo.getAllReferences
    getLineType(line, documentInfo.filePath) match
      case LandoLineType.EmptyLine => line
      case LandoLineType.Comment => line
      case LandoLineType.Requirement => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.Requirement), (ref: DocReference, l: String) => ref.originalLine.equals(l), (ref: DocReference) => ref.enrichedLine.get)
      case LandoLineType.Event => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.Event), (ref: DocReference, l: String) => ref.originalLine.equals(l), (ref: DocReference) => ref.enrichedLine.get)
      case LandoLineType.Scenario => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.Scenario), (ref: DocReference, l: String) => ref.originalLine.equals(l), (ref: DocReference) => ref.enrichedLine.get)
      case LandoLineType.Reference => extractEnrichedText(line, references.filter(_.referenceType == ReferenceType.Requirement), (ref: DocReference, l: String) => ref.originalLine.equals(l), (ref: DocReference) => ref.enrichedLine.get)
      case LandoLineType.Relation => extractEnrichedText(line, documentInfo.getRelations, (ref: DocRelation, l: String) => ref.originalLine.equals(l), (ref: DocRelation) => ref.enrichedLine.get)
      case LandoLineType.LineToBeSkipped => ""
  }

  private def enrichRelation(relation: DocRelation, references: Set[DocReference], docName: String): DocRelation = {
    val srcRefs = references.filter(ref => referenceNameMatches(relation.relationReference.sourceName, ref.referenceName))
    val trgRefs = references.filter(ref => referenceNameMatches(relation.relationReference.targetName, ref.referenceName))
    val enrichedLine = latexFormatter.enrichLineWithLabel(addRelation(relation, srcRefs, trgRefs, docName), relation.relationReference.relationName)

    relation.copy(enrichedLine = Some(enrichedLine))
  }

  private def getLineType(line: String, documentPath: String): LandoLineType = {
    val lowerLine = line.toLowerCase.strip()
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
    val referenceOption = getReferenceType(line, fileType)
    val referenceType = referenceOption.get
    val referenceName = extractReferenceName(line, referenceType, fileName)
    DocReference(
      fileName, referenceName, referenceType, DocumentType.Lando, line, Some(latexFormatter.enrichLineWithLabel(line, referenceName.reference))
    )
  }

  private def extractReferences(filePath: String): Set[DocReference] = {
    extract(filePath, isReference, transformReference)
  }

  private def extractEvents(filePath: String): Set[DocReference] = {
    if (getFileType(filePath) != FileType.EventFile) return Set.empty
    extract(filePath, isEvent, transformReference)
  }

  private def extractRequirements(filePath: String): Set[DocReference] = {
    if (getFileType(filePath) != FileType.RequirementFile) return Set.empty
    extract(filePath, isRequirement, transformReference)
  }

  private def extractScenarios(filePath: String): Set[DocReference] = {
    if (getFileType(filePath) != FileType.ScenarioFile) return Set.empty
    extract(filePath, isScenario, transformReference)
  }

  private def addRelation(relation: DocRelation, srcRefs: Set[DocReference], trgRefs: Set[DocReference], currentDoc: String): String = {
    "relation " + addHrefLink(srcRefs, relation.relationReference.sourceName, currentDoc) + " "
      + relation.relationType.toString + " " + addHrefLink(trgRefs, relation.relationReference.targetName, currentDoc)
  }

  private def enrichRelations(relations: Set[DocRelation], references: Set[DocReference], docName: String): Set[DocRelation] = {
    val enrichedRels = relations.map(rel => enrichRelation(rel, references, docName))
    assert(enrichedRels.forall(rel => rel.enrichedLine.isDefined))
    enrichedRels
  }

  private def isRelation(line: String, prev: String): Boolean = {
    (line.nonEmpty && !line.startsWith("//")) && getRelationType(line).nonEmpty
  }

  private def isReference(line: String, prev: String = ""): Boolean = {
    (line.nonEmpty && !line.startsWith("//")) && getReferenceType(line, FileType.ComponentFile).nonEmpty
  }

  private def isRequirement(line: String, previousLine: String): Boolean = {
    (line.nonEmpty && !line.startsWith("//")) && previousLine.isEmpty && getReferenceType(line, FileType.RequirementFile).nonEmpty
  }

  private def isEvent(line: String, previousLine: String): Boolean = {
    (line.nonEmpty && !line.startsWith("//")) && previousLine.isEmpty && getReferenceType(line, FileType.EventFile).nonEmpty
  }

  private def isScenario(line: String, previousLine: String): Boolean = {
    line.nonEmpty && !line.startsWith("//") && previousLine.isEmpty && getReferenceType(line, FileType.ScenarioFile).nonEmpty
  }

  private def extractReferenceName(line: String, referenceType: ReferenceType, documentName: String): ReferenceName = {
    val strippedLine = line.replace(referenceType.toString.toLowerCase, "").strip()

    val acronym = extractAcronym(strippedLine)
    val name = extractReferenceText(strippedLine, acronym, referenceType)

    val referenceName = s"${documentName}_${referenceType.toString}_${latexFormatter.sanitizeLine(name)}"
    ReferenceName(name, referenceName, acronym)
  }

  private def extractReferenceText(strippedLine: String, acronym: Option[String], referenceType: ReferenceType) = {
    val line = strippedLine.split(" ")
      .dropRight(if acronym.nonEmpty then 1 else 0)
      .mkString(" ").strip()

    if referenceType == ReferenceType.Scenario && line.exists(_.isDigit) then
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
    val relationName = relationType.toString + "_" + latexFormatter.sanitizeLine(source) + "_" + latexFormatter.sanitizeLine(target)
    RelationReference(source, target, relationName)
  }


  private def getReferenceType(line: String, fileType: FileType): Option[ReferenceType] = {
    fileType match
      case FileType.RequirementFile => Some(ReferenceType.Requirement)
      case FileType.ScenarioFile => Some(ReferenceType.Scenario)
      case FileType.EventFile => Some(ReferenceType.Event)
      case FileType.ViewFile => None
      case FileType.ComponentFile =>
        val lowerCaseLine = line.toLowerCase.strip()
        if lowerCaseLine.startsWith("component") then
          Some(ReferenceType.Component)
        else if lowerCaseLine.startsWith("subsystem") then
          Some(ReferenceType.SubSystem)
        else if lowerCaseLine.startsWith("system") then
          Some(ReferenceType.System)
        else
          None
  }

  private def getRelationType(line: String): Option[RelationType] = {
    val lowerCaseLine = line.toLowerCase
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
      DocRelation(fileName,
        true,
        relationReference,
        relationType,
        line,
        None)
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


