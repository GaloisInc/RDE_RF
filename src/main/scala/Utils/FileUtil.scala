package Utils

import org.apache.logging.log4j.scala.Logging

import java.io.File
import java.nio.file.{Files, Paths, StandardCopyOption}
import scala.io.{Codec, Source}
import scala.language.existentials
import scala.reflect.io.Directory

/**
 * Utility class for file operations
 */
object FileUtil extends Logging {
  // Method to remove all decorated files from a directory
  def deleteRecursivelyDecoratedFiles(path: String): Unit = {
    require(path.nonEmpty, "path must not be empty")
    require(new File(path).exists(), "path must exist")

    val directory = new Directory(new File(path))

    val decoratedFiles = directory.deepFiles.withFilter(
      f =>
        // must be latex file and the title must contain the word "decorated"
        f.extension == ".tex"
          && f.name.contains("decorated")
    ).toArray

    decoratedFiles.foreach(f => f.delete())

    logger.info(s"Deleted ${decoratedFiles.length} decorated files")
  }

  def writeFile(filePath: String, jsonString: String): Unit = {
    require(filePath.nonEmpty, "filePath must not be empty")
    require(jsonString.nonEmpty, "jsonString must not be empty")
    val file = new File(filePath)
    val parentDir = file.getParentFile
    if (!parentDir.exists()) parentDir.mkdirs()
    Files.write(Paths.get(filePath), jsonString.getBytes(Codec.UTF8.charSet))
  } ensuring (_ => new File(filePath).exists())

  def readFile(filePath: String): String = {
    require(Files.exists(Paths.get(filePath)), s"File $filePath does not exist")
    val source = Source.fromFile(filePath)(Codec.UTF8)
    val content = source.mkString
    source.close()
    content
  }


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
    val directory = new Directory(sourceDir)

    val files = directory.deepFiles.withFilter(
      f =>
        fileTypesOfTypesOfInterest.contains(f.extension)
    ).toArray
    files.map(_.path)
  } ensuring ((files: Array[String]) => files.forall(file => fileTypesOfTypesOfInterest.contains(getFileType(file)) && file.nonEmpty && FileUtil.fileExists(file)))

  def getFileName(path: String): String = {
    require(path.nonEmpty, "Path is empty")
    val fileName = path.split("/").takeRight(1).head.takeWhile(c => c != '.')
    fileName
  } ensuring ((fileName: String) => !fileName.contains(".") && path.contains(fileName) && fileName.nonEmpty)

  def getFileType(path: String): String = {
    require(path.nonEmpty, "Path is empty")
    require(path.contains('.'), "Path does not contain a file type" + path)
    require(path.split('.').takeRight(1).headOption.isDefined, s"Path $path does not contain a file type after the last dot")
    val fileType = path.split('.').takeRight(1).head
    if (fileType.matches("[a-zA-Z0-9]+")) fileType else {
      throw new Exception(s"File type $fileType is not valid")
    }
  } ensuring ((fileName: String) => {
    assert(fileName.nonEmpty, "File type is empty")
    assert(fileName.matches("^[a-zA-Z0-9_/]*$"), s"FileType $fileName contains illegal characters")
    assert(path.contains(fileName), s"File type $fileName is not contained in path $path")
    true
  })

  def getDocumentType(path: String): Types.DocumentType.Value = {
    getFileType(path) match {
      case "cry" => Types.DocumentType.Cryptol
      case ".icry" => Types.DocumentType.Cryptol
      case "saw" => Types.DocumentType.Saw
      case "lando" => Types.DocumentType.Lando
      case "lobot" => Types.DocumentType.Lobot
      case "sysml" => Types.DocumentType.SysML
      case "bsv" => Types.DocumentType.BSV
      case "sv" => Types.DocumentType.SV
      case "json" => Types.DocumentType.Fret
      case filePath => throw new IllegalArgumentException(s"File type not supported: $filePath on file $path")
    }
  }

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

    Control.using(Source.fromFile(path)(Codec.UTF8)) { source =>
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

  /**
   * Creates a directory if it does not exist
   */
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
    assert(FileUtil.fileExists(resPath), "File has not been moved to the desired location:" + resPath)
    true
  })
}
