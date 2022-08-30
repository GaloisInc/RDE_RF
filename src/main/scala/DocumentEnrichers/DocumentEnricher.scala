package DocumentEnrichers

import Types.*
import Types.DocumentType.Saw
import Utils.Control

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}
import scala.collection.mutable

abstract class DocumentEnricher {
  val fileUtil = new FileUtil()
  val latexFormatter = new LatexFormatter()

  def extractDocumentInfo(fileString: String): DocumentInfo

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

  def getFileType(path: String): FileType

  def formatLine(line: String, documentInfo: DocumentInfo): String

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

  protected def extractEnrichedText[A](line: String, references: Set[A], searchCriteria: (A, String) => Boolean, projectEnriched: A => String): String = {
    val relevantRefs = references.filter(ref => searchCriteria(ref, line))
    if relevantRefs.isEmpty
    then
      line
    else
      assert(relevantRefs.size == 1)
      projectEnriched(relevantRefs.head)
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
}





