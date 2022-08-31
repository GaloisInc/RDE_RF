package DocumentEnrichers

import Types.*
import Types.DocumentInfos.DocumentInfo
import Types.DocumentType.Saw
import Utils.{Control, FileUtil}

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}
import java.util.Locale
import scala.collection.mutable

abstract class DocumentEnricher(skipTodos: Boolean = false) {
  val fileUtil = new FileUtil()
  val latexFormatter = new LatexFormatter()

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
  } ensuring ((res: String) => res.length <= line.length && keyWordsToRemove.forall(unwantedKeyword => !res.contains(unwantedKeyword)))


  def enrichFile(documentInfo: DocumentInfo): String = {
    //documentChecker(documentInfo)
    val filePath = documentInfo.filePath
    val decoratedFilePath = fileUtil.decorateFileName(filePath)
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
        if line.isEmpty && lastLine.isEmpty
        then lastLine = line
        else
          writer.println(line)
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
    val fileName = fileUtil.getFileName(filePath)
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

  protected def extractEnrichedText[A <: EnrichableString](line: String, references: Set[A]): String = {
    val relevantRefs = references.filter(ref => ref.originalLine == line)
    if relevantRefs.isEmpty
    then
      line
    else
      assert(relevantRefs.size == 1)
      relevantRefs.headOption match
        case None => ""
        case Some(value) => value.enrichedLine match
          case Some(enrichedLine) => enrichedLine
          case None => ""
  }

  protected def referenceNameMatches(name: String, referenceName: ReferenceName): Boolean = {
    if (referenceName.acronym.isDefined) {
      name.equals(referenceName.acronym.get)
    } else {
      name.equals(referenceName.name)
    }
  }

  protected def addHrefLink(refs: Set[DocReference], name: String, currentDocument: String): String = {
    if (refs.nonEmpty) {
      val ref = refs.head
      latexFormatter.addReference(ref, currentDocument)
    } else {
      //No reference to add
      name
    }
  }

  protected def getReferenceType(line: String): Option[ReferenceType] = {
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

  private def matchKeyword(line: String, keyWord: String): Boolean = {
    require(keyWord.nonEmpty)
    line.startsWith(keyWord + " ")
  }

}





