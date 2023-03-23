package DocumentEnrichers

import Formatter.LatexFormatter
import Specs.FileSpecs
import Types.DocReference.DocReference
import Types.DocumentInfos.FretDocument
import Types.FRET.{FRETRequirement, FRETSemantics, FRETVariable}
import Types.{DocumentType, ReferenceName, ReferenceType}
import Utils.FileUtil
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.apache.logging.log4j.scala.Logging

class FRETDocumentEnricher(override val formatterType: LatexFormatter) extends DocumentEnricher[FretDocument](formatterType) with Logging {
  def parseDocument(fileString: String): FretDocument = {
    require(FileSpecs.fileChecks(Set(fileString), Set("json")), "filePath must be a sysml file")
    logger.info(s"Parsing file $fileString")
    val fileName = FileUtil.fileNameFromPath(fileString)

    val fileContent = FileUtil.readFile(fileString)

    val fretRequirements = parseRequirements(fileContent)
    val variables = parseVariables(fileContent)
    val requirements = fretRequirements.map(r =>
      new DocReference(fileName, 0, ReferenceName(r.reqid, None), ReferenceType.Requirement, DocumentType.Fret, r.asJson.spaces2SortKeys))
    val types = variables.map(v =>
      new DocReference(fileName, 0, ReferenceName(v.variable_name, None), ReferenceType.Type, DocumentType.Fret, v.asJson.spaces2SortKeys))

    logger.info("Finished parsing file " + fileString)
    FretDocument(fileName, fileString, fretRequirements, variables, requirements, types)
  }

  private implicit val RequirementEncoder: Encoder[FRETRequirement] = (a: FRETRequirement) => Json.obj(
    ("reqid", Json.fromString(a.reqid)),
    ("parent_reqid", Json.fromString(a.parent_reqid)),
    ("rationale", Json.fromString(a.rationale)),
    ("fulltext", Json.fromString(a.fulltext.mkString("\n"))),
    ("description", Json.fromString(a.semantics.description.mkString("\n")))
  )

  private implicit val VariableEncoder: Encoder[FRETVariable] = (a: FRETVariable) => Json.obj(
    ("variable_name", Json.fromString(a.variable_name)),
    ("dataType", Json.fromString(a.dataType)),
    ("idType", Json.fromString(a.idType)),
    ("completed", Json.fromBoolean(a.completed)),
    ("modeldoc", Json.fromBoolean(a.modeldoc))
  )

  private implicit val SemanticsDecoder: Decoder[FRETSemantics] = (c: HCursor) => for {
    description <- c.downField("description").as[String]
  } yield FRETSemantics(description = description.split("\n").toList)

  private implicit val RequirementDecoder: Decoder[FRETRequirement] = (c: HCursor) => for {
    reqid <- c.downField("reqid").as[String]
    parent_reqid <- c.downField("parent_reqid").as[String]
    rationale <- c.downField("rationale").as[String]
    fulltext <- c.downField("fulltext").as[String]
    semantics <- c.downField("semantics").as[FRETSemantics]
  } yield FRETRequirement(reqid, parent_reqid, rationale, fulltext.split("\n").toList, semantics)

  private implicit val VariableDecoder: Decoder[FRETVariable] = (c: HCursor) => for {
    variable_name <- c.downField("variable_name").as[String]
    dataType <- c.downField("dataType").as[String]
    idType <- c.downField("idType").as[String]
    completed <- c.downField("completed").as[Boolean]
    modeldoc <- c.downField("modeldoc").as[Boolean]
  } yield FRETVariable(variable_name, dataType, idType, completed, modeldoc)

  override def decorateFile(document: FretDocument): String = {
    logger.info(s"Decorating file ${document.filePath}")
    val filePath = document.filePath
    val decoratedFilePath = FileUtil.decorateFileName(filePath)
    val requirementString = document.docRequirements.map(
      req => req.enrich(latexFormatter)).mkString("\n")
    val variableString = document.docVariables.map(
      var_ => var_.enrich(latexFormatter)).mkString("\n")
    FileUtil.writeFile(decoratedFilePath, requirementString + variableString)
    decoratedFilePath
  }

  private def parseRequirements(jsonString: String): List[FRETRequirement] = {
    require(jsonString.nonEmpty, "jsonString must not be empty")
    val fretDocument = io.circe.parser.decode[List[FRETRequirement]](jsonString)
    fretDocument match {
      case Left(_) => List.empty[FRETRequirement]
      case Right(requirements) => requirements
    }
  }

  private def parseVariables(jsonString: String): List[FRETVariable] = {
    require(jsonString.nonEmpty, "jsonString must not be empty")
    val fretDocument = io.circe.parser.decode[List[FRETVariable]](jsonString)
    fretDocument match {
      case Left(_) => List.empty[FRETVariable]
      case Right(fretDocument) => fretDocument
    }
  }
}


