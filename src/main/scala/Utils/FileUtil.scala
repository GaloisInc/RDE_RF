package Utils

import Types.DocumentInfos.DocumentInfo
import Types.DocumentType

import java.io.File
import java.nio.file.{Files, Path, Paths, StandardCopyOption}

object FileUtil {
  def fileExists(file: String): Boolean = {
    val path = Paths.get(file)
    Files.exists(path)
  }

  def findSourceFiles(sourcePath: String, fileTypesOfTypesOfInterest: Set[String]): Array[String] = {
    val sourceDir = new File(sourcePath)
    require(sourceDir.exists(), "Source directory does not exist")
    require(sourceDir.isDirectory, "Source path is not a directory")

    var sourceFiles = List.empty[String]
    Files.walk(Paths.get(sourcePath))
      .filter(Files.isRegularFile(_))
      .filter(p => fileTypesOfTypesOfInterest.exists(p.toFile.getName.endsWith))
      .map(_.toFile.getAbsolutePath)
      .forEach(sourceFiles ::= _)

    sourceFiles.toArray
    //    sourceDir.listFiles.filter(f => f.isFile && fileTypesOfTypesOfInterest.exists(f.getName.endsWith)).map(_.getAbsolutePath)
  }

  def getLandoDocuments(enrichedDocuments: Array[DocumentInfo]): Array[DocumentInfo] = {
    enrichedDocuments.filter(doc => doc.documentType == DocumentType.Lando)
  } ensuring ((docs: Array[DocumentInfo]) => docs.toSet.subsetOf(enrichedDocuments.toSet) && docs.forall(_.documentType == DocumentType.Lando))

  def getSysMLDocuments(enrichedDocuments: Array[DocumentInfo]): Array[DocumentInfo] = {
    enrichedDocuments.filter(doc => doc.documentType == DocumentType.SysML)
  } ensuring ((docs: Array[DocumentInfo]) => docs.toSet.subsetOf(enrichedDocuments.toSet) && docs.forall(_.documentType == DocumentType.SysML))

  def getCryptolDocuments(enrichedDocuments: Array[DocumentInfo]): Array[DocumentInfo] = {
    enrichedDocuments.filter(doc => doc.documentType == DocumentType.Cryptol)
  } ensuring ((docs: Array[DocumentInfo]) => docs.toSet.subsetOf(enrichedDocuments.toSet) && docs.forall(_.documentType == DocumentType.Cryptol))

  def getBlusSpecDocuments(enrichedDocuments: Array[DocumentInfo]): Array[DocumentInfo] = {
    enrichedDocuments.filter(doc => doc.documentType == DocumentType.BSV)
  } ensuring ((docs: Array[DocumentInfo]) => docs.toSet.subsetOf(enrichedDocuments.toSet) && docs.forall(_.documentType == DocumentType.BSV))

  def getSystemVerilogDocumetns(enrichedDocuments: Array[DocumentInfo]): Array[DocumentInfo] = {
    enrichedDocuments.filter(doc => doc.documentType == DocumentType.SV)
  } ensuring ((docs: Array[DocumentInfo]) => docs.toSet.subsetOf(enrichedDocuments.toSet) && docs.forall(_.documentType == DocumentType.SV))


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
    //require(Files.exists(Paths.get(path)))
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

  def isOfFileType(path: String, filetype: String): Boolean = {
    require(path.nonEmpty)
    Control.using(io.Source.fromFile(path)) { source =>
      source.getLines().exists(_.startsWith(filetype))
    }
  }

  def getFilesInDirectory(dir: String): Set[String] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      val filesToDelete = d.listFiles.filter(file => file.getName.contains("decorated") || file.getName.contains("DS_Store"))
      filesToDelete.foreach(file => file.delete())
      d.listFiles.filter(file => file.isFile).map(_.toString).toSet
    } else {
      Set.empty[String]
    }
  }

  def moveRenameFile(source: String, destinationDirectory: String): String = {
    require(source.nonEmpty)
    require(Files.exists(Paths.get(source)))
    val fileName = source.split("/").takeRight(1).head

    val directory = new File(destinationDirectory)
    if (!directory.exists) {
      directory.mkdir
      // If you require it to make the entire directory path including parents,
      // use directory.mkdirs(); here instead.
    }

    val path = Files.move(
      Paths.get(source),
      Paths.get(Paths.get(destinationDirectory, fileName).toString),
      StandardCopyOption.REPLACE_EXISTING
    )
    path.toString
  }
}
