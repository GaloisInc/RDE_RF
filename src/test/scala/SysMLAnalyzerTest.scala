import Utils.Control
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import java.io.File
import scala.collection.mutable
import scala.io.Source

class SysMLAnalyzerTest extends AnyFlatSpec with should.Matchers {
  def getListOfFiles(dir: String): List[String] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      val filesToDelete = d.listFiles.filter(file => file.getName.contains("decorated"))
      filesToDelete.foreach(file => file.delete())
      d.listFiles.filter(_.isFile).map(_.toString).toList
    } else {
      List[String]()
    }
  }

  "SysMLReader" should "to enrich references across documents" in {
    val sysmlDocuments = getClass.getResource("SysML").getPath
    val landoDocuments = getClass.getResource("Lando").getPath

    val filesToAnalyze = getListOfFiles(sysmlDocuments).toArray ++ getListOfFiles(landoDocuments).toArray

    val analyzedDocumentsLando = DocumentAnalyzer.enrichFiles(filesToAnalyze)
  }

  "SysMLReader" should "be able to find non specialized constructs" in {
    val sysmlDocuments = getClass.getResource("SysML").getPath
    val landoDocuments = getClass.getResource("Lando").getPath

    val filesToAnalyze = getListOfFiles(sysmlDocuments).toArray ++ getListOfFiles(landoDocuments).toArray

    val nonSecializedConstructs = DocumentAnalyzer.nonSpecializedLandoConstructs(filesToAnalyze)

    val analyzedDocumentsLando = DocumentAnalyzer.enrichFiles(filesToAnalyze)

  }

  "SysMLReader" should "be able to clean file" in {
    val sysmlDocuments = getClass.getResource("SysML").getPath
    val landoDocuments = getClass.getResource("Lando").getPath

    val filesToAnalyze = getListOfFiles(sysmlDocuments).toArray ++ getListOfFiles(landoDocuments).toArray

    val analyzedDocumentsLando = DocumentAnalyzer.cleanUpUnusedGlossaries(filesToAnalyze)
  }
}



