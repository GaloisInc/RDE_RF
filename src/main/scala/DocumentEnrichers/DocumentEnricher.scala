package DocumentEnrichers

import Formatter.{InlineFormatter, LatexFormatter, ReferenceFormatter}
import Types.*
import Types.DocumentInfos.DocumentInfo
import Types.DocumentType.Saw
import Utils.{Control, FileUtil}

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}
import java.util.Locale
import scala.collection.mutable
import scala.util.matching.Regex

abstract class DocumentEnricher(val formatterType: LatexFormatter,
                                val skipTodos: Boolean = false) {
  val latexFormatter = new ReferenceFormatter(formatterType)


  def parseDocument(fileString: String): DocumentInfo

  def formatLine(line: String, documentInfo: DocumentInfo): String

  def decorateFile(documentInfo: DocumentInfo): String = {
    val filePath = documentInfo.filePath
    val decoratedFilePath = FileUtil.decorateFileName(filePath)
    val decoratedFile = new File(decoratedFilePath) // Temporary File
    val writer = new PrintWriter(decoratedFile)
    var lastLine = ""
    Control.using(io.Source.fromFile(filePath)) { source => {
      source.getLines()
        .map(line => {
          if skipTodos && line.contains("@todo")
          then ""
          else
            formatLine(line, documentInfo)
        }
        ).foreach(line => {
        // To Remove Multiple empty lines
        if line.isEmpty && lastLine.isEmpty
        then lastLine = line
        else
          val highLighted = highlightURLLinks(line)
          writer.println(highLighted)
          lastLine = line
      })
    }
    }
    writer.close()
    decoratedFile.getPath
  }

  def enrichDocuments(filesToAnalyze: Array[String]): Array[String] = {
    filesToAnalyze.indices.map(idx => {
      val currentFile = filesToAnalyze(idx)
      val documentInfo = parseDocument(currentFile)
      decorateFile(documentInfo)
    }).toArray
  } ensuring ((res: Array[String]) => res.length == filesToAnalyze.length)

  protected def extractEnrichedText[A <: EnrichableString](line: String, references: Set[A]): String = {
    val relevantRefs = references.filter(ref => ref.originalLine == line)
    if relevantRefs.isEmpty
    then
      line
    else
      relevantRefs.headOption match
        case None => ""
        case Some(value) => value.enrichedLine(latexFormatter)
  }

  def highlightURLLinks(str: String): String = {
    val urlRegex = Regex("https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\\b([-a-zA-Z0-9()@:%_\\+.~#?&//=]*)")
    val urls = urlRegex findAllIn str
    if urls.isEmpty then str
    else urls.foldLeft(str)((line, url) => {
      val formattedUrl = latexFormatter.createWebLink(url)
      line.replace(url, formattedUrl)
    })
  } ensuring((highlightedString: String) => highlightedString.length >= str.length,
    s"Highlighted link is shorter. " +
      s"Original Link: $str")

}





