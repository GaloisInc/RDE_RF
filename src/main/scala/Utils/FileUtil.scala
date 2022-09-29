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
      .filter(f => f.toString.nonEmpty)
      .filter(p => fileOfCorrectType(p.toFile.toString, fileTypesOfTypesOfInterest))
      .map(_.toFile.getAbsolutePath)
      .forEach(f => sourceFiles = f :: sourceFiles)

    sourceFiles.toArray
  } ensuring ((files: Array[String]) => files.forall(file => fileTypesOfTypesOfInterest.contains(getFileType(file)) && file.nonEmpty && FileUtil.fileExists(file)))


  def fileOfCorrectType(file: String, fileTypes: Set[String]): Boolean = {
    require(file.nonEmpty, "file must not be empty")
    require(fileTypes.nonEmpty, "fileTypes must not be empty")
    if (fileExists(file) && file.contains('.') && file.split('.').lastOption.isDefined && file.split('.').lastOption.get.matches("[a-zA-Z0-9]+")) {
      fileTypes.contains(getFileType(file))
    } else {
      false
    }
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
    require(path.contains('.'), "Path does not contain a file type" + path)
    require(path.split('.').takeRight(1).headOption.isDefined, s"Path ${path} does not contain a file type after the last dot")
    val fileType = path.split('.').takeRight(1).head
    fileType.matches("[a-zA-Z0-9]+") match {
      case true => fileType
      case false => throw new Exception(s"File type ${fileType} is not valid")
    }
  } ensuring ((fileName: String) => {
    assert(fileName.nonEmpty, "File type is empty")
    assert(fileName.matches("^[a-zA-Z0-9_/]*$"), s"FileType $fileName contains illegal characters")
    assert(path.contains(fileName), s"File type $fileName is not contained in path $path")
    true
  })

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
    require(new File(path).isFile, "Path is not a file")
    require(new File(path).canRead, "File is not readable")

    Control.using(io.Source.fromFile(path)(io.Codec.UTF8)) { source =>
      source.getLines().exists(_.startsWith(filetype))
    }
  }

  def allFilesReadable(files: Array[String]): Boolean = {
    require(files.nonEmpty, "Files is empty")
    files.forall(file => new File(file).canRead)
  }

  def getNonReadableFiles(files: Array[String]): Array[String] = {
    require(files.nonEmpty, "Files is empty")
    files.filter(file => !new File(file).canRead)
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

  def createDirectory(dir: String): Unit = {
    require(dir.nonEmpty, "Directory name is empty")
    val d = new File(dir)
    if (!d.exists) {
      d.mkdirs()
    }
  }

  def moveRenameFile(source: String, destinationDirectory: String): String = {
    require(source.nonEmpty, "Source path is empty")
    require(FileUtil.fileExists(source), "Source file does not exist at path" + source)
    val fileName = source.split("/").takeRight(1).head

    createDirectory(destinationDirectory)

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
