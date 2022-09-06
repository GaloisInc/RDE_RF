import Analyzers.DocumentAnalyzer
import Utils.{Control, FileUtil}
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import java.nio.file.Path
import scala.collection.mutable
import scala.io.Source

class DocumentAnalyerSpec extends AnyFlatSpec with should.Matchers {
  private val fileUtil: FileUtil = FileUtil()

  "CryptolReader" should "to enrich references across documents" in {
    val sysmlDocuments = getClass.getResource("SysML").getPath
    val landoDocuments = getClass.getResource("Lando").getPath
    val cryptolDocuments = getClass.getResource("Cryptol").getPath

    val filesToAnalyze = fileUtil.getListOfFiles(sysmlDocuments).toArray
      ++ fileUtil.getListOfFiles(landoDocuments).toArray
      ++ fileUtil.getListOfFiles(cryptolDocuments).toArray

    DocumentAnalyzer.enrichAndSortFiles(filesToAnalyze)
  }

}



