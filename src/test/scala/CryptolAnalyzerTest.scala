import Utils.Control
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.collection.mutable
import scala.io.Source
import DocumentEnrichers.FileUtil

import java.nio.file.Path

class CryptolAnalyzerTest extends AnyFlatSpec with should.Matchers {
  private val fileUtil: FileUtil = FileUtil()

  "CryptolReader" should "to enrich references across documents" in {
    val sysmlDocuments = getClass.getResource("SysML").getPath
    val landoDocuments = getClass.getResource("Lando").getPath
    val cryptolDocuments = getClass.getResource("Cryptol").getPath

    val filesToAnalyze = fileUtil.getListOfFiles(sysmlDocuments).toArray
      ++ fileUtil.getListOfFiles(landoDocuments).toArray
      ++ fileUtil.getListOfFiles(cryptolDocuments).toArray

    val decoratedFiles = DocumentAnalyzer.enrichFiles(filesToAnalyze)

    val decoratedLandoFiles = decoratedFiles.filter(_.endsWith("lando"))
    val decoratedSysMLFiles = decoratedFiles.filter(_.endsWith("sysml"))
    val decoratedCryptolFiles = decoratedFiles.filter(_.endsWith("cry"))

    decoratedLandoFiles.foreach(filePath => {
      val destinationPath = Path.of(fileUtil.getDirectory(filePath), "decorated").toString
      fileUtil.moveRenameFile(filePath, destinationPath)
    })
    decoratedSysMLFiles.foreach(filePath => {
      val destinationPath = Path.of(fileUtil.getDirectory(filePath), "decorated").toString
      fileUtil.moveRenameFile(filePath, destinationPath)
    })

    decoratedCryptolFiles.foreach(filePath => {
      val destinationPath = Path.of(fileUtil.getDirectory(filePath), "decorated").toString
      fileUtil.moveRenameFile(filePath, destinationPath)
    })
  }

}



