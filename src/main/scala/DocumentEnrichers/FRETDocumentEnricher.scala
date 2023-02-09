package DocumentEnrichers

import Types.DocumentInfos.DocumentInfo
import Types.FRET.{FRETRequirement, FRETSemantics}
import Utils.FileUtil
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}


class FRETDocumentEnricher() {
  private implicit val FRETSemanticsEncoder: Encoder[FRETRequirement] = new Encoder[FRETRequirement] {
    final def apply(a: FRETRequirement): Json = Json.obj(
      ("reqid", Json.fromString(a.reqid)),
      ("parent_reqid", Json.fromString(a.parent_reqid)),
      ("rationale", Json.fromString(a.rationale)),
      ("fulltext", Json.fromString(a.fulltext)),
      ("description", Json.fromString(a.semantics.description))
    )
  }


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
  } yield {
    FRETRequirement(reqid, parent_reqid, rationale, fulltext, semantics)
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

  def parseJsonToFretDocument(jsonString: String): List[FRETRequirement] = {
    require(jsonString.nonEmpty, "jsonString must not be empty")
    val fretDocument = io.circe.parser.decode[List[FRETRequirement]](jsonString)
    fretDocument match {
      case Left(error) => throw new Exception(error.getMessage)
      case Right(fretDocument) => fretDocument
    }
  }

  def formatFile(inputFile: String, outputFile: String): Unit = {
    require(inputFile.nonEmpty, "filePath must not be empty")
    require(FileUtil.getFileType(inputFile) == "json", "filePath must be a json file")
    require(FileUtil.fileExists(inputFile), "filePath must exist")
    require(outputFile.nonEmpty, "outputFile must not be empty")
    require(outputFile != inputFile, "outputFile must not be the same as inputFile")

    val fretDocument = parseJsonToFretDocument(FileUtil.readFile(inputFile))
    val jsonString = fretDocument.asJson.spaces2
    FileUtil.writeFile(outputFile, jsonString)
  }ensuring(FileUtil.fileExists(outputFile), "outputFile must exist")

}


