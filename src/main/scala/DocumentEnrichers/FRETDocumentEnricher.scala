package DocumentEnrichers

import Formatter.LatexFormatter
import Types.DocumentInfos.FretDocument
import Types.FRET.{FRETRequirement, FRETSemantics}
import Utils.FileUtil
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.apache.logging.log4j.scala.Logging

class FRETDocumentEnricher(override val formatterType: LatexFormatter) extends DocumentEnricher[FretDocument](formatterType) with Logging {
  def parseDocument(fileString: String): FretDocument = {
    require(fileString.nonEmpty, "File path cannot be empty")
    require(FileUtil.getFileType(fileString) == "json", "File type must be FRET")
    require(FileUtil.fileExists(fileString), "filePath must exist")
    logger.info(s"Parsing file $fileString")

    val fretRequirements = parseJsonToFretDocument(FileUtil.readFile(fileString))
    val fileName = FileUtil.getFileName(fileString)

    logger.info("Finished parsing file " + fileString)

    new FretDocument(fileName, fileString, fretRequirements)
  }


  private implicit val FRETSemanticsEncoder: Encoder[FRETRequirement] = (a: FRETRequirement) => Json.obj(
    ("reqid", Json.fromString(a.reqid)),
    ("parent_reqid", Json.fromString(a.parent_reqid)),
    ("rationale", Json.fromString(a.rationale)),
    ("fulltext", Json.fromString(a.fulltext)),
    ("description", Json.fromString(a.semantics.description))
  )

  private implicit val FRETSemanticsDecoder: Decoder[FRETSemantics] = (c: HCursor) => for {
    description <- c.downField("description").as[String]
  } yield {
    FRETSemantics(
      description = description
    )
  }

  private implicit val nestedDecoder: Decoder[FRETRequirement] = (c: HCursor) => for {
    reqid <- c.downField("reqid").as[String]
    parent_reqid <- c.downField("parent_reqid").as[String]
    rationale <- c.downField("rationale").as[String]
    fulltext <- c.downField("fulltext").as[String]
    semantics <- c.downField("semantics").as[FRETSemantics]
  } yield FRETRequirement(reqid, parent_reqid, rationale, fulltext, semantics)

  override def decorateFile(document: FretDocument): String = {
    logger.info(s"Decorating file ${document.filePath}")
    val filePath = document.filePath
    val decoratedFilePath = FileUtil.decorateFileName(filePath)
    val jsonString = document.requirements.asJson.spaces2
    FileUtil.writeFile(decoratedFilePath, jsonString)
    decoratedFilePath
  }


  /*
def parseDocument(filePath: String): FRETDocumentInfo = {
  require(filePath.nonEmpty, "filePath must not be empty")
  require(FileUtil.getFileType(filePath) == "json", "filePath must be a json file")
  require(FileUtil.fileExists(filePath), "filePath must exist")

  implicit val nestedDecoder: Decoder[FRETRequirement] = deriveDecoder[FRETRequirement]
  implicit val FRETSemanticsDecoder: Decoder[FRETSemantics] = deriveDecoder[FRETSemantics]
  implicit val jsonDecoder: Decoder[FRETScope] = deriveDecoder[FRETScope]
  implicit val decoder: Decoder[FretDocument] = deriveDecoder[FretDocument]

  val fretDocument = io.circe.parser.decode[FretDocument](FileUtil.readFile(filePath))
  fretDocument match {
    case Left(error) => throw new Exception(error.getMessage)
    case Right(fretDocument) => {
      val fileName = FileUtil.getFileName(filePath)
      val fretDocumentInfo = new FRETDocumentInfo(fileName, filePath, fretDocument)
      fretDocumentInfo
    }
  }
}
*/

  private def parseJsonToFretDocument(jsonString: String): List[FRETRequirement] = {
    require(jsonString.nonEmpty, "jsonString must not be empty")
    val fretDocument = io.circe.parser.decode[List[FRETRequirement]](jsonString)
    fretDocument match {
      case Left(error) => throw new Exception(error.getMessage)
      case Right(fretDocument) => fretDocument
    }
  }

  /**
   * Extracts the enriched text from a line
   *
   * @param line Line to be enriched
   * @return Enriched text
   */
  def formatLine(line: String, documentInfo: FretDocument): String = line
}


