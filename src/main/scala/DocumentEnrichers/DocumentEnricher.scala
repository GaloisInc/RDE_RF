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

abstract class DocumentEnricher(val formatterType: LatexFormatter = new InlineFormatter(),
                                val skipTodos: Boolean = false) {
  val latexFormatter = new ReferenceFormatter(formatterType)

  def keyWordsToRemove: Array[String]

  //require(keyWordsToRemove.forall(_.forall(_.isLower)), "All keywords are lower case")

  def keyWordsToReference: ReferenceKeyWords


  def extractDocumentInfo(fileString: String): DocumentInfo

  def getFileType(path: String): FileType

  def formatLine(line: String, documentInfo: DocumentInfo): String

  def removeKeyWords(line: String): String = {
    keyWordsToRemove.foldRight(line)((keyWord, l) => {
      l.replaceAll(keyWord, "")
    }).strip()
      .replaceAll(" +", " ")
  } ensuring((res: String) => res.length <= line.length
    && keyWordsToRemove.forall(unwantedKeyword => !res.contains(unwantedKeyword)), "The result should not contain any of the keywords")


  def enrichFile(documentInfo: DocumentInfo): String = {
    //documentChecker(documentInfo)
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
          val highLighted = highlightLinks(line)
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
      val documentInfo = extractDocumentInfo(currentFile)
      enrichFile(documentInfo)
    }).toArray
  } ensuring ((res: Array[String]) => res.length == filesToAnalyze.length)

  def extract[A](filePath: String, filter: (String, String) => Boolean, transformer: (String, String, FileType) => A): Set[A] = {
    require(filePath.nonEmpty)
    //require(fileCheck(filePath))
    val fileName = FileUtil.getFileName(filePath)
    val fileType = getFileType(filePath)
    Control.using(io.Source.fromFile(filePath)) { source => {
      val lines = source.getLines().toArray
      lines
        .indices.filter(idx => {
        val line = lines(idx)
        val prevInd = if idx > 0 then idx - 1 else idx
        val previousLine = lines(prevInd)
        filter(line, previousLine)
      }).map(idx => {
        val line = lines(idx)
        transformer(line, fileName, fileType)
      }).toSet
    }
    }
  }


  protected def extractEnrichedText[A <: EnrichableString ](line: String, references: Set[A]): String = {
    val relevantRefs = references.filter(ref => ref.originalLine == line)
    if relevantRefs.isEmpty
    then
      line
    else
    // assert(relevantRefs.size == 1, "There should be only one reference per line")
      relevantRefs.headOption match
        case None => ""
        case Some(value) => value.enrichedLine(latexFormatter)
  }

  protected def getReferenceType(line: String): Option[ReferenceType] = {
    def matchKeyword(line: String, keyWord: String): Boolean = {
      require(keyWord.nonEmpty)
      line.startsWith(keyWord + " ")
    }

    val lowerCaseLine = removeKeyWords(line).toLowerCase(Locale.US).strip()
    if matchKeyword(lowerCaseLine, keyWordsToReference.System) then
      Some(ReferenceType.System)
    else if matchKeyword(lowerCaseLine, keyWordsToReference.SubSystem) then
      Some(ReferenceType.SubSystem)
    else if matchKeyword(lowerCaseLine, keyWordsToReference.Component) then
      Some(ReferenceType.Component)
    else if matchKeyword(lowerCaseLine, keyWordsToReference.Scenario) then
      Some(ReferenceType.Scenario)
    else if matchKeyword(lowerCaseLine, keyWordsToReference.Requirement) then
      Some(ReferenceType.Requirement)
    else if matchKeyword(lowerCaseLine, keyWordsToReference.Event) then
      Some(ReferenceType.Event)
    else if matchKeyword(lowerCaseLine, keyWordsToReference.Connection) then
      Some(ReferenceType.Connection)
    else if matchKeyword(lowerCaseLine, keyWordsToReference.Import) then
      Some(ReferenceType.Import)
    else if matchKeyword(lowerCaseLine, keyWordsToReference.View) then
      Some(ReferenceType.View)
    else if matchKeyword(lowerCaseLine, keyWordsToReference.ViewPoint) then
      Some(ReferenceType.ViewPoint)
    else if matchKeyword(lowerCaseLine, keyWordsToReference.Attribute) then
      Some(ReferenceType.Attribute)
    else if matchKeyword(lowerCaseLine, keyWordsToReference.Type) then
      Some(ReferenceType.Type)
    else
      None
  }

  def highlightLinks(str: String): String = {
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





