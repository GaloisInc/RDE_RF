package DocumentEnrichers

import Formatter.{LatexFormatter, ReferenceFormatter}
import Types.DecorateableString
import Types.DocumentInfos.DocumentInfo
import Utils.{Control, FileUtil}
import org.apache.logging.log4j.scala.Logging

import java.io.{File, PrintWriter}
import scala.io.{Codec, Source}

abstract class DocumentEnricher[T <: DocumentInfo[T]](val formatterType: LatexFormatter,
                                                      val skipTodos: Boolean = false) extends Logging {
  val latexFormatter = new ReferenceFormatter(formatterType)

  /**
   * Parses a document and returns a DocumentInfo
   *
   * @param fileString Path to the file
   * @return DocumentInfo
   */
  def parseDocument(fileString: String): T

  /**
   * Extracts the enriched text from a line
   *
   * @param line Line to be enriched
   * @return Enriched text
   */
  def formatLine(line: String, documentInfo: T): String = line

  /**
   * Decorates a file with the enriched text
   *
   * @param documentInfo DocumentInfo
   * @return Path to the decorated file
   */
  def decorateFile(documentInfo: T): String = {
    logger.info(s"Decorating file ${documentInfo.filePath}")
    val filePath = documentInfo.filePath
    val decoratedFilePath = FileUtil.decorateFileName(filePath)
    val decoratedFile = new File(decoratedFilePath) // Temporary File
    val writer = new PrintWriter(decoratedFile)
    var lastLine = ""
    Control.using(
      Source.fromFile(filePath)(Codec.UTF8)) { source => {
      source.getLines()
        .map(line => {
          if (skipTodos && line.contains("@todo")) ""
          else
            formatLine(line, documentInfo)
        }
        ).foreach(line => {
        // To Remove Multiple empty lines
        if (line.isEmpty && lastLine.isEmpty) lastLine = line
        else {
          val highLighted = highlightURLLinks(line)
          writer.println(highLighted)
          lastLine = line
        }
      })
    }
    }
    writer.close()
    decoratedFile.getPath
  }

  protected def extractEnrichedText[A <: DecorateableString](line: String, references: Set[A]): String = {
    val relevantRefs = references.filter(ref => ref.originalLine == line)
    if (relevantRefs.isEmpty) line
    else
      relevantRefs.headOption match {
        case Some(ref) => ref.enrich(latexFormatter)
        case None => ""
      }
  }

  private def highlightURLLinks(str: String): String = {
    val urlRegex = """https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\\b([-a-zA-Z0-9()@:%_\\+.~#?&//=]*)""".r
    val urls = urlRegex findAllIn str
    if (urls.isEmpty) str
    else urls.foldLeft(str)((line, url) => {
      val formattedUrl = latexFormatter.createWebLink(url).toLatex
      line.replace(url, formattedUrl)
    })
  } ensuring((highlightedString: String) => highlightedString.length >= str.length,
    s"Highlighted link is shorter. " +
      s"Original Link: $str")

}





