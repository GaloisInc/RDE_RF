package Utils

import Types.DocumentInfos.DocumentInfo
import Types.DocumentType

import java.io.File
import java.nio.file.{Files, Paths, StandardCopyOption}

object FileUtil {
  def fileExists(file: String): Boolean = {
    require(file.nonEmpty, "file must not be empty")
    val path = Paths.get(file)
    Files.exists(path)
  }

  def findSourceFiles(sourcePath: String, fileTypesOfTypesOfInterest: Set[String]): Array[String] = {
    require(fileTypesOfTypesOfInterest.nonEmpty, "fileTypesOfTypesOfInterest must not be empty")
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
    require(path.nonEmpty, "Path is empty")
    val fileName = path.split("/").takeRight(1).head.takeWhile(c => c != '.')
    fileName
  } ensuring ((fileName: String) => !fileName.contains(".") && path.contains(fileName) && fileName.nonEmpty)

  def getFileType(path: String): String = {
    require(path.nonEmpty, "Path is empty")
    val fileType = path.split('.').takeRight(1).head
    fileType
  } ensuring ((fileName: String) => !fileName.contains("/") && !fileName.contains(".") && path.contains(fileName) && fileName.nonEmpty)


  def getDirectory(path: String): String = {
    require(path.nonEmpty)
    //require(Files.exists(Paths.get(path)))
    val directory = path.split('/').dropRight(1).mkString("/")
    directory
  } ensuring ((directory: String) => path.startsWith(directory) && path.length > directory.length)

  def decorateFileName(path: String): String = {
    require(path.nonEmpty, "Path is empty")
    require(FileUtil.fileExists(path), "File does not exist at path" + path)
    val directory = getDirectory(path)
    val decoratedFileName = getFileName(path) + "_decorated." + getFileType(path)
    val decoratedFilePath = Paths.get(directory, decoratedFileName)
    decoratedFilePath.toString
  } ensuring ((resPath: String) => resPath.contains("_decorated") && getFileType(resPath) == getFileType(path) && resPath.contains(getFileName(path)))

  def isOfFileType(path: String, filetype: String): Boolean = {
    require(path.nonEmpty, "Path is empty")
    require(FileUtil.fileExists(path), "File does not exist at path" + path)
    Control.using(io.Source.fromFile(path)) { source =>
      source.getLines().exists(_.startsWith(filetype))
    }
  }

  def getFilesInDirectory(dir: String): Set[String] = {
    require(dir.nonEmpty, "Directory is empty")
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
    require(source.nonEmpty, "Source path is empty")
    require(FileUtil.fileExists(source), "Source file does not exist at path" + source)
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
  } ensuring ((resPath: String) => {
    val fileName = source.split("/").takeRight(1).head
    assert(resPath.contains(fileName) && resPath.contains(destinationDirectory), "File was not moved to destination directory")
    assert(FileUtil.fileExists(resPath), "File does not exist at path" + resPath)
    assert(!FileUtil.fileExists(source), "File still exists at path" + source)
    true
  })
}
