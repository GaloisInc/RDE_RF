import Analyzers.{DocumentAnalyzer, LatexDocumentData}
import Utils.{Control, FileUtil}
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers._

import java.nio.file.Path
import scala.collection.mutable
import scala.io.Source
import Report.PaperLayout
import Formatter.{InlineFormatter, ReferenceFormatter}

class DocumentAnalyerSpec extends AnyFlatSpec with should.Matchers {
  "CryptolReader" should "to enrich references across documents" in {
    val sysmlDocuments = getClass.getResource("SysML").getPath
    val landoDocuments = getClass.getResource("Lando").getPath
    val cryptolDocuments = getClass.getResource("Cryptol").getPath

    val filesToAnalyze = FileUtil.getListOfFiles(sysmlDocuments).toArray ++
      FileUtil.getListOfFiles(landoDocuments).toArray ++
      FileUtil.getListOfFiles(cryptolDocuments).toArray

    val targetFolder = getClass.getResource("").getPath
    val title = "Test"

    val latexDocumentation = LatexDocumentData(title, targetFolder, PaperLayout.A4, new InlineFormatter())
    DocumentAnalyzer.enrichAndSortFiles(filesToAnalyze, latexDocumentation)
  }

  "CryptolReader" should "to enrich SYSML references" in {
    val sysmlDocuments = getClass.getResource("SysML").getPath
    val landoDocuments = getClass.getResource("Lando").getPath
    val cryptolDocuments = getClass.getResource("Cryptol").getPath

    val filesToAnalyze = FileUtil.getListOfFiles(sysmlDocuments).toArray ++
      FileUtil.getListOfFiles(landoDocuments).toArray ++
      FileUtil.getListOfFiles(cryptolDocuments).toArray

    val targetFolder = getClass.getResource("").getPath
    val title = "Test"

    val latexDocumentation = LatexDocumentData(title, targetFolder, PaperLayout.A4, new InlineFormatter())
    val report = DocumentAnalyzer.enrichAndSortFiles(filesToAnalyze, latexDocumentation)

    val formatter = new ReferenceFormatter(new InlineFormatter())

    val documentInfo = report.sysmlDocuments.filter(_.documentName.equalsIgnoreCase("RTS_Glossary")).head

    val referencesWithActualReferences = documentInfo.getAllReferences.filter(_.getReferences.nonEmpty)

    val reference = referencesWithActualReferences.find(ref => ref.getName.equalsIgnoreCase("Synthesizer")).get
    val enrichedLineSynthesize = reference.enrichedLine(formatter)

    assert(enrichedLineSynthesize.contains("\\hyperref"), "Synthesizer not enriched")

    val referenceASIC = referencesWithActualReferences.find(ref => ref.getName.equalsIgnoreCase("ASIC")).get
    val enrichedLineASIC = referenceASIC.enrichedLine(formatter)

    assert(enrichedLineASIC.contains("\\hyperref"), "ASIC not enriched")

    val referenceUniversalSerialBus = referencesWithActualReferences.find(ref => ref.getName.equalsIgnoreCase("Universal Serial Bus")).get
    val enrichedLineUniversalSerialBus = referenceUniversalSerialBus.enrichedLine(formatter)

    assert(enrichedLineUniversalSerialBus.contains("\\hyperref"), "Universal Serial Bus not enriched with reference.")
  }

}



