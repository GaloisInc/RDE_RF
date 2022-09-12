import Analyzers.{DocumentAnalyzer, LatexDocumentData}
import Utils.{Control, FileUtil}
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import java.nio.file.Path
import scala.collection.mutable
import scala.io.Source
import Report.PaperLayout
import Formatter.InlineFormatter

class DocumentAnalyerSpec extends AnyFlatSpec with should.Matchers {
  "CryptolReader" should "to enrich references across documents" in {
    val sysmlDocuments = getClass.getResource("SysML").getPath
    val landoDocuments = getClass.getResource("Lando").getPath
    val cryptolDocuments = getClass.getResource("Cryptol").getPath

    val filesToAnalyze = FileUtil.getListOfFiles(sysmlDocuments).toArray
      ++ FileUtil.getListOfFiles(landoDocuments).toArray
      ++ FileUtil.getListOfFiles(cryptolDocuments).toArray

    val targetFolder = getClass.getResource("").getPath
    val title = "Test"

    val latexDocumentation = LatexDocumentData(title, targetFolder, PaperLayout.A4, InlineFormatter())
    DocumentAnalyzer.enrichAndSortFiles(filesToAnalyze, latexDocumentation)
  }

}



