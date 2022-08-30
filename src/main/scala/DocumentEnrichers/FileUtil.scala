package DocumentEnrichers

import Types.{DocumentInfo, DocumentType}
import Utils.Control

import java.nio.file.{Files, Paths}

class FileUtil {
  def getLandoDocuments(enrichedDocuments: Array[DocumentInfo]): Array[DocumentInfo] = {
    enrichedDocuments.filter(doc => doc.documentType == DocumentType.Lando)
  } ensuring ((docs: Array[DocumentInfo]) => docs.toSet.subsetOf(enrichedDocuments.toSet))

  def getSysMLDocuments(enrichedDocuments: Array[DocumentInfo]): Array[DocumentInfo] = {
    enrichedDocuments.filter(doc => doc.documentType == DocumentType.SysML)
  } ensuring ((docs: Array[DocumentInfo]) => docs.toSet.subsetOf(enrichedDocuments.toSet))

  def getCryptolDocuments(enrichedDocuments: Array[DocumentInfo]): Array[DocumentInfo] = {
    enrichedDocuments.filter(doc => doc.documentType == DocumentType.Cryptol)
  } ensuring ((docs: Array[DocumentInfo]) => docs.toSet.subsetOf(enrichedDocuments.toSet))


  def getFileName(path: String): String = {
    require(path.nonEmpty)
    val fileName = path.split("/").takeRight(1).head.takeWhile(c => c != '.')
    fileName
  } ensuring ((fileName: String) => !fileName.contains(".") && path.contains(fileName))

  def getFileType(path: String): String = {
    require(path.nonEmpty)
    val fileType = path.split('.').takeRight(1).head
    fileType
  } ensuring ((fileName: String) => !fileName.contains("/") && path.contains(fileName))


  def getDirectory(path: String): String = {
    require(path.nonEmpty)
    require(Files.exists(Paths.get(path)))
    val directory = path.split('/').dropRight(1).mkString("/")
    directory
  } ensuring ((directory: String) => path.startsWith(directory) && path.length > directory.length)

  def decorateFileName(path: String): String = {
    require(path.nonEmpty)
    require(Files.exists(Paths.get(path)))
    val directory = getDirectory(path)
    val decoratedFileName = getFileName(path) + "_decorated." + getFileType(path)
    val decoratedFilePath = Paths.get(directory, decoratedFileName)
    decoratedFilePath.toString
  } ensuring ((resPath: String) => resPath.contains("_decorated") && getFileType(resPath) == getFileType(path) && resPath.contains(getFileName(path)))

  def isFileType(path: String, filetype: String): Boolean = {
    require(path.nonEmpty)
    Control.using(io.Source.fromFile(path)) { source =>
      source.getLines().exists(_.startsWith(filetype))
    }
  }
}
