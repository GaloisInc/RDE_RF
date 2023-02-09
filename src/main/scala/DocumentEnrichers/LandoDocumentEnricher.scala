package DocumentEnrichers

import Formatter.LatexFormatter
import Types.DocReference.DocReference
import Types.DocumentInfos.{DocumentInfo, LandoDocumentInfo}
import Types._
import Utils.Matcher.referenceNameMatches
import Utils.{Control, FileUtil}
import org.apache.logging.log4j.scala.Logging

import scala.io.{Codec, Source}
import scala.util.matching.Regex

class LandoDocumentEnricher(override val formatterType: LatexFormatter,
                            override val skipTodos: Boolean = true) extends DocumentEnricher[LandoDocumentInfo](formatterType, skipTodos) with Logging {


  val relationRegex: Regex = """^relation\s*(?:(.*?)\s+(contains|client|inherit))\s+(.*)""".r
  //All components, systems and subsystems are referenced by their name which start with a capital letter
  val componentRegex: Regex = """^component\s+([A-Z].*?)(?:\s+\((.*)\))?""".r
  val systemRegex: Regex = """^system\s+([A-Z].*?)(?:\s+\((.*)\))?""".r
  val subsystemRegex: Regex = """^subsystem\s+([A-Z].*?)(?:\s+\((.*)\))?""".r

  def parseDocument(filePath: String): LandoDocumentInfo = {
    require(filePath.nonEmpty, "filePath must not be empty")
    require(FileUtil.getFileType(filePath) == "lando", "filePath must be a lando file")
    require(FileUtil.fileExists(filePath), "filePath must exist")

    logger.info(s"Start parsing lando file $filePath")
    val references: Set[DocReference] = extractReferences(filePath, FileType.ComponentFile)
    val relations: Set[DocRelation] = extractRelations(filePath)
    val requirements: Set[DocReference] = extractReferences(filePath, FileType.RequirementFile)
    val scenarios: Set[DocReference] = extractReferences(filePath, FileType.ScenarioFile)
    val events: Set[DocReference] = extractReferences(filePath, FileType.EventFile)

    val fileName = FileUtil.getFileName(filePath)
    val enrichedRelations = enrichRelations(relations, references, fileName)

    logger.info(s"Finished parsing lando file $filePath")
    new LandoDocumentInfo(fileName, filePath, references, enrichedRelations, events, requirements, scenarios)
  } ensuring ((landoDoc: LandoDocumentInfo) =>
    landoDoc.documentType == DocumentType.Lando
      && landoDoc.filePath == filePath
      && landoDoc.documentName == FileUtil.getFileName(filePath))

  override def formatLine(line: String, documentInfo: LandoDocumentInfo): String = {
    val references = documentInfo.getAllReferences
    val referenceTypesOfComponent = Set(ReferenceType.Component, ReferenceType.System, ReferenceType.SubSystem)
    getLineType(line, documentInfo.filePath) match {
      case LandoLineType.EmptyLine => line
      case LandoLineType.Comment => line
      case LandoLineType.LineToBeSkipped => ""
      case LandoLineType.Relation => extractEnrichedText(line, documentInfo.getRelations)
      case LandoLineType.Requirement => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.Requirement))
      case LandoLineType.Event => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.Event))
      case LandoLineType.Scenario => extractEnrichedText(line, references.filter(_.getReferenceType == ReferenceType.Scenario))
      case LandoLineType.Reference => extractEnrichedText(line, references.filter(ref => referenceTypesOfComponent.contains(ref.getReferenceType)))
    }
  }

  private def enrichRelation(relation: DocRelation, references: Set[DocReference], docName: String): DocRelation = {
    require(references.nonEmpty, "references must not be empty")
    val sourceReference = references.filter(ref => referenceNameMatches(relation.getSourceName, ref.getReferenceName))
    val targetReference = references.filter(ref => referenceNameMatches(relation.getTargetName, ref.getReferenceName))

    //assert(sourceReference.nonEmpty, s"Relation source reference not found: ${relation.getSourceName} in $docName")
    //assert(targetReference.nonEmpty, s"Relation target reference not found: ${relation.getTargetName} in $docName")

    relation.copy(
      sourceRef = sourceReference.headOption,
      targetRef = targetReference.headOption
    )
  }


  private def getLineType(line: String, documentPath: String): LandoLineType.Value = {
    val lowerLine = line.trim()
    if (lowerLine.isEmpty) return LandoLineType.EmptyLine
    if (lowerLine.startsWith("//")) return LandoLineType.Comment
    if (lowerLine.startsWith("relation")) return LandoLineType.Relation
    if (lowerLine.startsWith("component") || lowerLine.startsWith("subsystem") || lowerLine.startsWith("system")) return LandoLineType.Reference
    if (line.startsWith("@todo")) return LandoLineType.LineToBeSkipped
    getFileType(documentPath) match {
      case FileType.RequirementFile => LandoLineType.Requirement
      case FileType.ScenarioFile => LandoLineType.Scenario
      case FileType.EventFile => LandoLineType.Event
      case FileType.ComponentFile => LandoLineType.Reference
    }
  }

  private def transformReference(line: String, fileName: String, fileType: FileType.Value): DocReference = {
    val referenceOption = getReferenceTypeBasedOnFileType(line, fileType)
    val getReferenceType = referenceOption.get
    val referenceName = extractReferenceName(line)
    new DocReference(
      fileName, referenceName, getReferenceType, DocumentType.Lando, line
    )
  }

  private def extractReferences(filePath: String, fileType: FileType.fileType): Set[DocReference] = {
    if (getFileType(filePath) != fileType) return Set.empty
    val fileChecker = (l: String, p: String) => isOfType(fileType, l, p)
    extract(filePath, fileChecker, transformReference)
  }

  private def enrichRelations(relations: Set[DocRelation], references: Set[DocReference], docName: String): Set[DocRelation] = {
    val enrichedRels = relations.map(rel => enrichRelation(rel, references, docName))
    enrichedRels
  }

  private def isRelation(line: String, prev: String): Boolean = {
    line match {
      case relationRegex(_, _, _) => true
      case _ => false
    }
  }

  private def isOfType(fileType: FileType.fileType, line: String, previousLine: String): Boolean = {
    line.nonEmpty && !line.startsWith("//") &&
      (fileType == FileType.ComponentFile ||
        (fileType != FileType.ComponentFile && (previousLine.isEmpty || previousLine.startsWith("//")))) &&
      getReferenceTypeBasedOnFileType(line, fileType).nonEmpty
  }

  def extractReferenceName(line: String): ReferenceName = {
    def noneIfNull(s: String): Option[String] = Option(s)

    val strippedLine = line.trim()
    strippedLine match {
      case componentRegex(name, acronym) => ReferenceName(name.trim(), noneIfNull(acronym))
      case systemRegex(name, acronym) => ReferenceName(name.trim(), noneIfNull(acronym))
      case subsystemRegex(name, acronym) => ReferenceName(name.trim(), noneIfNull(acronym))
      case _ =>
        // Events, scenarios and requirements do have an acronym
        ReferenceName(strippedLine)
    }
  }

  private def getReferenceTypeBasedOnFileType(line: String, fileType: FileType.Value): Option[ReferenceType.Value] = {
    fileType match {
      case FileType.RequirementFile
      => if (!line.startsWith("requirements")) Some(ReferenceType.Requirement) else None
      case FileType.ScenarioFile
      => if (!line.startsWith("scenarios")) Some(ReferenceType.Scenario) else None
      case FileType.EventFile
      => if (!line.startsWith("events")) Some(ReferenceType.Event) else None
      case FileType.ComponentFile =>
        line match {
          case componentRegex(_, _)
          => Some(ReferenceType.Component)
          case systemRegex(_, _)
          => Some(ReferenceType.System)
          case subsystemRegex(_, _)
          => Some(ReferenceType.SubSystem)
          case _ => None
        }
    }
  }

  // Enhance this
  private def getRelationType(symbol: String): Option[RelationTypes.Value] = {
    symbol match {
      case "client" => Some(RelationTypes.client)
      case "contains" => Some(RelationTypes.contains)
      case "inherit" => Some(RelationTypes.inherit)
      case _ => None
    }
  }

  def extractRelations(filePath: String): Set[DocRelation] = {
    def transformRelation(line: String, fileName: String, fileType: FileType.Value): DocRelation = {
      line match {
        case relationRegex(source, symbol, target) =>
          new DocRelation(
            fileName, RelationReference(source.trim(), target.trim()),
            getRelationType(symbol).get, line, None, None)
        case _ => throw new IllegalArgumentException(s"Could not extract relation name from line: $line")

      }
    }

    extract(filePath, isRelation, transformRelation)
  }

  def getFileType(path: String): FileType.Value = {
    require(path.nonEmpty, "path must not be empty")
    if (FileUtil.isOfFileType(path, "events")) return FileType.EventFile
    if (FileUtil.isOfFileType(path, "requirements")) return FileType.RequirementFile
    if (FileUtil.isOfFileType(path, "scenarios")) return FileType.ScenarioFile
    FileType.ComponentFile
  }

  def extract[A](filePath: String, filter: (String, String) => Boolean, transformer: (String, String, FileType.Value) => A): Set[A] = {
    require(filePath.nonEmpty, "The file path should not be empty")
    val fileName = FileUtil.getFileName(filePath)
    val fileType = getFileType(filePath)
    Control.using(Source.fromFile(filePath)((Codec.UTF8))) { source => {
      val lines = source.getLines().toArray
      lines
        .indices.filter(idx => {
        val line = lines(idx)
        val prevInd = if (idx > 0) idx - 1 else idx
        val previousLine = lines(prevInd)
        filter(line, previousLine)
      }).map(idx => {
        val line = lines(idx)
        transformer(line, fileName, fileType)
      }).toSet
    }
    }
  }
}


