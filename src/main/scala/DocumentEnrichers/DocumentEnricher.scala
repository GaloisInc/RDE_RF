package DocumentEnrichers

import Formatter.{LatexFormatter, ReferenceFormatter}
import Specs.FileSpecs
import Types.DocumentInfos.DocumentInfo
import Types.EnrichableString
import Utils.{Control, FileUtil}
import org.apache.logging.log4j.scala.Logging

import java.io.{File, PrintWriter}
import scala.io.{Codec, Source}

abstract class DocumentEnricher(val formatterType: LatexFormatter,
                                val skipTodos: Boolean = false) extends Logging {
  val latexFormatter = new ReferenceFormatter(formatterType)
  def parseDocument(fileString: String): DocumentInfo

  def formatLine(line: String, documentInfo: DocumentInfo): String

  /**
   * Decorates a file with the enriched text
   *
   * @param documentInfo DocumentInfo
   * @return Path to the decorated file
   */
  def decorateFile(documentInfo: DocumentInfo): String = {
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

  def enrichDocuments(filesToAnalyze: Array[String]): Array[String] = {
    FileSpecs.allFilesExist(filesToAnalyze.toSet)
    filesToAnalyze.indices.map(idx => {
      val currentFile = filesToAnalyze(idx)
      val documentInfo = parseDocument(currentFile)
      decorateFile(documentInfo)
    }).toArray
  } ensuring ((res: Array[String]) => res.length == filesToAnalyze.length)

  protected def extractEnrichedText[A <: EnrichableString](line: String, references: Set[A]): String = {
    val relevantRefs = references.filter(ref => ref.originalLine == line)
    if (relevantRefs.isEmpty) line
    else
      relevantRefs.headOption match {
        case Some(ref) => ref.enrichedLine(latexFormatter)
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





