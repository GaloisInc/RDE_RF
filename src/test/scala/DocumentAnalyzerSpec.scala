import Analyzers.{DocumentAnalyzer, LatexDocumentData}
import ConfigParser.{FileDocRef, RefinementLoader, RefinementModel}
import Formatter.{InlineFormatter, ReferenceFormatter}
import Report.PaperLayout
import Types.DocumentType
import Utils.{FileUtil, ResourceFiles}
import org.scalatest.flatspec._
import org.scalatest.matchers._

class DocumentAnalyzerSpec extends AnyFlatSpec with should.Matchers {
  private val authorName: String = "TestAuthor"
  private val targetFolder = getClass.getResource("").getPath


  "CryptolReader" should "to enrich references across documents" in {
    val filesToAnalyze = ResourceFiles.getFilesOfTypes(Set(DocumentType.SysML, DocumentType.Lando, DocumentType.Cryptol, DocumentType.Fret))
    val title = "Test"
    val latexDocumentation = LatexDocumentData(title, authorName, targetFolder, PaperLayout.A4, new InlineFormatter())
    DocumentAnalyzer.generateReport(filesToAnalyze, latexDocumentation, Set.empty[RefinementModel])
  }

  "CryptolReader" should "to enrich SYSML references" in {
    val filesToAnalyze = ResourceFiles.getFilesOfTypes(Set(DocumentType.SysML, DocumentType.Lando, DocumentType.Cryptol, DocumentType.Fret))
    val title = "Test"

    val latexDocumentation = LatexDocumentData(title, authorName, targetFolder, PaperLayout.A4, new InlineFormatter())
    val report = DocumentAnalyzer.generateReport(filesToAnalyze, latexDocumentation, Set.empty[RefinementModel])

    val formatter = new ReferenceFormatter(new InlineFormatter())

    val documentInfo = report.documents.sysmlDocuments.filter(_.documentName.equalsIgnoreCase("RTS_Glossary")).head

    val referencesWithActualReferences = documentInfo.getAllReferences.filter(_.getReferences.nonEmpty)

    val reference = referencesWithActualReferences.find(ref => ref.getName.equalsIgnoreCase("Synthesizer")).get
    val enrichedLineSynthesize = reference.enrich(formatter)

    assert(enrichedLineSynthesize.contains("\\hyperref"), "Synthesizer not enriched")

    val referenceASIC = referencesWithActualReferences.find(ref => ref.getName.equalsIgnoreCase("ASIC")).get
    val enrichedLineASIC = referenceASIC.enrich(formatter)

    assert(enrichedLineASIC.contains("\\hyperref"), "ASIC not enriched")

    val referenceUniversalSerialBus = referencesWithActualReferences.find(ref => ref.getName.equalsIgnoreCase("Universal Serial Bus")).get
    val enrichedLineUniversalSerialBus = referenceUniversalSerialBus.enrich(formatter)

    assert(enrichedLineUniversalSerialBus.contains("\\hyperref"), "Universal Serial Bus not enriched with reference.")
  }

  "DocumentAnalyzer" should "be able to add explicit References" in {
    val references: Set[RefinementModel] = Set(
      RefinementModel(FileDocRef("acronyms", "Commercial Off The Shelf"), FileDocRef("RTS_Glossary", "ASIC")),
      RefinementModel(FileDocRef("acronyms", "Continuous Verification"), FileDocRef("RTS_Glossary", "Coq"))
    )

    val filesToAnalyze = ResourceFiles.getFilesOfTypes(Set(DocumentType.SysML, DocumentType.Lando, DocumentType.Cryptol, DocumentType.Fret))

    val title = "Test"
    val latexDocumentation = LatexDocumentData(title, authorName, targetFolder, PaperLayout.A4, new InlineFormatter())
    val report = DocumentAnalyzer.generateReport(filesToAnalyze, latexDocumentation, references)

    val acronymLando = report.documents.landoDocuments.find(_.documentName.equalsIgnoreCase("acronyms")).get
    val glossarySysML = report.documents.sysmlDocuments.find(_.documentName.equalsIgnoreCase("RTS_Glossary")).get

    acronymLando.getAllReferences.exists(ref => ref.getName.equalsIgnoreCase("Commercial Off The Shelf")) should be(true)
    val CTOSRef = acronymLando.getAllReferences.find(ref => ref.getName.equalsIgnoreCase("Commercial Off The Shelf")).get
    CTOSRef.getRefinements.nonEmpty should be(true)
    CTOSRef.getRefinements.get.exists(ref => ref.getName.equalsIgnoreCase("ASIC")) should be(true)

    val CVRef = acronymLando.getAllReferences.find(ref => ref.getName.equalsIgnoreCase("Continuous Verification")).get
    CVRef.getRefinements.nonEmpty should be(true)
    CVRef.getRefinements.get.exists(ref => ref.getName.equalsIgnoreCase("Coq")) should be(true)

    glossarySysML.getAllReferences.exists(ref => ref.getName.equalsIgnoreCase("Coq")) should be(true)
    val coqRef = glossarySysML.getAllReferences.find(ref => ref.getName.equalsIgnoreCase("Coq")).get
    coqRef.getAbstractions.nonEmpty should be(true)
    coqRef.getAbstractions.get.exists(ref => ref.getName.equalsIgnoreCase("Continuous Verification")) should be(true)

    val ASICRef = glossarySysML.getAllReferences.find(ref => ref.getName.equalsIgnoreCase("ASIC")).get
    ASICRef.getAbstractions.nonEmpty should be(true)
    ASICRef.getAbstractions.get.exists(ref => ref.getName.equalsIgnoreCase("Commercial Off The Shelf")) should be(true)
  }


  "DocumentAnalyzer" should "be able to add explicit References from file" in {
    val conf = getClass.getResource("refinementExamples/ExplicitReferences.conf")

    val references = RefinementLoader.load(conf.getPath).explicit_refinements.values.flatten.toSet

    val filesToAnalyze = ResourceFiles.getFilesOfTypes(Set(DocumentType.SysML, DocumentType.Lando, DocumentType.Cryptol, DocumentType.Fret))

    val title = "Test"
    val latexDocumentation = LatexDocumentData(title, authorName, targetFolder, PaperLayout.A4, new InlineFormatter())
    val report = DocumentAnalyzer.generateReport(filesToAnalyze, latexDocumentation, references)

    val testScenario = report.documents.landoDocuments.find(_.documentName.equalsIgnoreCase("test_scenarios")).get

    val testScenarioRef = testScenario.getAllReferences.find(ref => ref.getName.equalsIgnoreCase("Exceptional Behavior 2a - Cause Temperature Sensor 1 to Fail")).get
    testScenarioRef.getRefinements.nonEmpty should be(true)
    testScenarioRef.getRefinements.get.exists(ref => ref.getName.equalsIgnoreCase("2a - Cause Temperature Sensor 1 to Fail")) should be(true)
  }

  "DocumentAnalyzer" should "be able to add explicit References from source directory" in {
    val sourceFolder = getClass.getResource("Source").getPath
    val conf = getClass.getResource("refinementExamples/ExplicitReferences.conf")
    val references = RefinementLoader.load(conf.getPath).explicit_refinements.values.flatten.toSet
    val fileTypesOfTypesOfInterest = Set("lando", "sysml", "cry", "bsv", "sv", "json")

    val filesToAnalyze = FileUtil.findSourceFiles(sourceFolder, fileTypesOfTypesOfInterest)

    val title = "Test_From_Source_Directory"
    val latexDocumentation = LatexDocumentData(title, authorName, targetFolder, PaperLayout.A4, new InlineFormatter())
    val report = DocumentAnalyzer.generateReport(filesToAnalyze.toSet, latexDocumentation, references)

    val testScenario = report.documents.landoDocuments.find(_.documentName.equalsIgnoreCase("test_scenarios")).get

    val testScenarioRef = testScenario.getAllReferences.find(ref => ref.getName.equalsIgnoreCase("Exceptional Behavior 2a - Cause Temperature Sensor 1 to Fail")).get
    testScenarioRef.getRefinements.nonEmpty should be(true)
    testScenarioRef.getRefinements.get.exists(ref => ref.getName.equalsIgnoreCase("2a - Cause Temperature Sensor 1 to Fail")) should be(true)
  }


  "DocumentAnalyzer" should "be able to add explicit References from file and create Document with files" in {
    val conf = getClass.getResource("refinementExamples/ExplicitReferences.conf")
    val references = RefinementLoader.load(conf.getPath).explicit_refinements.values.flatten.toSet
    val filesToAnalyze = ResourceFiles.getFilesOfTypes(Set(DocumentType.SysML, DocumentType.Lando, DocumentType.Cryptol, DocumentType.Fret))

    val title = "Test Enriched with Explicit References"
    val latexDocumentation = LatexDocumentData(title, authorName, targetFolder, PaperLayout.A4, new InlineFormatter())
    val report = DocumentAnalyzer.generateReport(filesToAnalyze, latexDocumentation, references)
    report.buildDocumentationReport
  }
}



