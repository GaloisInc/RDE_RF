package DocumentEnrichers

import Formatter.{LatexFormatter, ReferenceFormatter}
import Types.DocReference.DocReference
import Types.DocumentInfos.FRETDocumentInfo
import Types.FRET.{FRETRequirement, FRETSemantics}
import Types.{DocumentType, ReferenceName, ReferenceType}
import Utils.FileUtil
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}


object FretJsonMethods {
  implicit val FRETSemanticsEncoder: Encoder[FRETRequirement] = (a: FRETRequirement) => Json.obj(
    ("reqid", Json.fromString(a.reqid)),
    ("parent_reqid", Json.fromString(a.parent_reqid)),
    ("rationale", Json.fromString(a.rationale)),
    ("fulltext", Json.fromString(a.fulltext)),
    ("description", Json.fromString(a.semantics.description))
  )

  implicit val FRETSemanticsDecoder: Decoder[FRETSemantics] = (c: HCursor) => for {
    description <- c.downField("description").as[String]
  } yield {
    FRETSemantics(
      description = description
    )
  }

  implicit val nestedDecoder: Decoder[FRETRequirement] = (c: HCursor) =>
    for {
      reqid <- c.downField("reqid").as[String]
      parent_reqid <- c.downField("parent_reqid").as[String]
      rationale <- c.downField("rationale").as[String]
      fulltext <- c.downField("fulltext").as[String]
      semantics <- c.downField("semantics").as[FRETSemantics]
    } yield {
      FRETRequirement(reqid, parent_reqid, rationale, fulltext, semantics)
    }


  def isFretDocument(jsonString: String): Boolean = {
    require(jsonString.nonEmpty, "jsonString must not be empty")
    val fretDocument = io.circe.parser.decode[List[FRETRequirement]](jsonString)
    fretDocument.isRight
  }
  def parseJsonToFretDocument(jsonString: String): List[FRETRequirement] = {
    require(jsonString.nonEmpty, "jsonString must not be empty")
    require(isFretDocument(jsonString), "jsonString must be a valid FRET document")
    io.circe.parser.decode[List[FRETRequirement]](jsonString).fold(_ => List(), x => x)
  }
}

object FRETDocumentEnricher {
  def parseDocument(filePath: String): FRETDocumentInfo = {
    import FretJsonMethods._
    require(filePath.nonEmpty, "filePath must not be empty")
    require(FileUtil.getFileType(filePath) == "json", "filePath must be a json file")
    require(FileUtil.fileExists(filePath), "filePath must exist")

    val fileName = FileUtil.getFileName(filePath)
    val fileContent = FileUtil.readFile(filePath)
    val fretRequirements = parseJsonToFretDocument(fileContent)

    val docRequirements = fretRequirements.map(
      fret_req =>
        new DocReference(
          documentName = fileName,
          referenceName = ReferenceName(fret_req.reqid, None),
          referenceType = ReferenceType.Requirement,
          originalLine = fret_req.asJson.spaces2,
          documentType = DocumentType.FRET
        )
    )
    new FRETDocumentInfo(
      documentName = fileName,
      filePath = filePath,
      documentType = DocumentType.FRET,
      requirements = docRequirements.toSet
    )
  }

  def createDecoratedFile(document: FRETDocumentInfo, outputFile: String, formatter: LatexFormatter): String = {
    val latexFormatter = new ReferenceFormatter(formatter)

    val fretRequirements = document.getAllReferences.map(
      docRef => {
        val formattedJson = docRef.originalLine
        val refinementString = docRef.refinementString(latexFormatter)
        val abstractString = docRef.abstrationString(latexFormatter)
        val lineWithLabel = latexFormatter.enrichLineWithLabel(formattedJson, docRef.getLabelText)
        lineWithLabel + refinementString + abstractString
      })

    val formattedFile = fretRequirements.mkString("[", ",\n", "]")
    FileUtil.writeFile(outputFile, formattedFile)
    outputFile
  } ensuring(FileUtil.fileExists(outputFile), "outputFile must exist")

}


