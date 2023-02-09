import Analyzers.DocumentAnalyzer
import DocumentEnrichers.SysMLDocumentEnricher
import Formatter.{InlineFormatter, ReferenceFormatter}
import Types.DocReference.DocReference
import Types.{DocumentType, ReferenceName, ReferenceType}
import Utils.TestUtilitySysml
import org.scalatest.flatspec._
import org.scalatest.matchers._

class SysMLAnalyzerTest extends AnyFlatSpec with should.Matchers {
  private val formatterType = new InlineFormatter()
  private val documentEnricher = new SysMLDocumentEnricher(formatterType)
  private val expectedDocumentType = DocumentType.SysML
  private val resourceFolder = "../SysML"


  "SysMLReader" should "to extract DocReference" in {
    val fileName = "PartsAndItems"
    val line1 = "abstract item id BISL 'Behavioral Interface Specification Language';;"

    val extractedReference1 = documentEnricher.transformReference(line1, fileName)
    val expectedReference1 = new DocReference(fileName, ReferenceName("Behavioral Interface Specification Language", Some("BISL")), ReferenceType.Component, DocumentType.SysML, line1)
    extractedReference1.isDefined should be(true)
    extractedReference1.get.getName should be(expectedReference1.getName)

    val line2 = "requirement 'Requirements Colloquial Completeness' : 'NRC Characteristic';"
    val expectedReference2 = new DocReference(fileName, ReferenceName("Requirements Colloquial Completeness"), ReferenceType.Requirement, DocumentType.SysML, line2)
    val extractedReference2 = documentEnricher.transformReference(line2, fileName)
    extractedReference2.isDefined should be(true)
    extractedReference2.get.getName should be(expectedReference2.getName)

    val line3 = "package id Glossary 'Project Glossary' {"
    val expectedReference3 = new DocReference(fileName, ReferenceName("Project Glossary", Some("Glossary")), ReferenceType.System, DocumentType.SysML, line3)
    val extractedReference3 = documentEnricher.transformReference(line3, fileName)
    extractedReference3.isDefined should be(true)
    extractedReference3.get.getName should be(expectedReference3.getName)


    val line4 = "abstract part def id SWImpl 'Hand-written Software Implementation'"
    val expectedReference4 = new DocReference(fileName, ReferenceName("Hand-written Software Implementation", Some("SWImpl")), ReferenceType.Component, DocumentType.SysML, line4)
    val extractedReference4 = documentEnricher.transformReference(line4, fileName)
    extractedReference4.isDefined should be(true)
    extractedReference4.get.getName should be(expectedReference4.getName)

    val line5 = "part def SAWscript;"
    val expectedReference5 = new DocReference(fileName, ReferenceName("SAWscript"), ReferenceType.Component, DocumentType.SysML, line5)
    val extractedReference5 = documentEnricher.transformReference(line5, fileName)
    extractedReference5.isDefined should be(true)
    extractedReference5.get.getName should be(expectedReference5.getName)

    val line6 = "item def 'RTS User';"
    val expectedReference6 = new DocReference(fileName, ReferenceName("RTS User"), ReferenceType.Component, DocumentType.SysML, line6)
    val extractedReference6 = documentEnricher.transformReference(line6, fileName)
    extractedReference6.get.getName should be(expectedReference6.getName)
    extractedReference6.get.getReferenceType should be(expectedReference6.getReferenceType)

    val line7 = "abstract item def 'Consistent Model' :> Consistent, Model;"
    val expectedReference7 = new DocReference(fileName, ReferenceName("Consistent Model"), ReferenceType.Component, DocumentType.SysML, line7)
    val extractedReference7 = documentEnricher.transformReference(line7, fileName)
    extractedReference7.get.getName should be(expectedReference7.getName)
    extractedReference7.get.getReferenceType should be(expectedReference7.getReferenceType)
  }

  "SysMLReader" should "to extract parts and items" in {
    val fileName = "RTS_Glossary"
    TestUtilitySysml.checkExtractReferences("RTS_Glossary", documentEnricher, expectedDocumentType, resourceFolder, numberExpectedSystem = 1, numberOfSubSystem = 80, numberOfComponent = 66)
  }

  "SysMLReader" should "to extract requirements" in {
    val fileName = "RTS_Requirements"
    TestUtilitySysml.checkExtractReferences(fileName, documentEnricher, expectedDocumentType, resourceFolder, 4, 0, 0, 17)
  }

  "SysMLReader" should "to extract packages" in {
    val fileName = "HARDENS"
    TestUtilitySysml.checkExtractReferences(fileName, documentEnricher, expectedDocumentType, resourceFolder, 7)
  }

  "SysMLReader" should "to extract Scenarios" in {
    val fileName = "RTS_Scenarios"
    TestUtilitySysml.checkExtractReferences(fileName, documentEnricher, expectedDocumentType, resourceFolder, 4, 0, 1, numberOfScenarios = 39)
  }

  "SysMLReader" should "to extract Views" in {
    val fileName = "RTS_Viewpoints"
    TestUtilitySysml.checkExtractReferences(fileName, documentEnricher, expectedDocumentType, resourceFolder, numberExpectedSystem = 1, numberOfViews = 8)
  }

  "SysMLReader" should "to extract Actions" in {
    val fileName = "RTS_Actions"
    TestUtilitySysml.checkExtractReferences(fileName, documentEnricher, expectedDocumentType, resourceFolder, numberExpectedSystem = 4, numberOfEvents = 20)
  }

  "SysMLReader" should "to extract and Enrich Glossary" in {
    val fileName = "RTS_Glossary"
    val file = getClass.getResource(s"SysML/$fileName.sysml").getFile
    val documentInfo = documentEnricher.parseDocument(file)
    val referencesWithReferences = documentInfo.getAllReferences.filter(_.isReferencingAnything)
    assert(referencesWithReferences.exists(ref => ref.getName.equalsIgnoreCase("Synthesizer")), "Synthesizer not found")
    DocumentAnalyzer.addReferences(documentInfo, documentInfo.getAllReferences)
    val referencesWithActualReferences = documentInfo.getAllReferences.filter(_.getReferences.nonEmpty)
    assert(referencesWithActualReferences.exists(ref => ref.getName.equalsIgnoreCase("Synthesizer")), "Synthesizer not found")

    val formatter = new ReferenceFormatter(new InlineFormatter())

    val reference = referencesWithActualReferences.find(ref => ref.getName.equalsIgnoreCase("Synthesizer")).get
    val enrichedLineSynthesize = reference.enrich(formatter)

    assert(enrichedLineSynthesize.contains("\\hyperref"), "Synthesizer not enriched")

    val referenceASIC = referencesWithActualReferences.find(ref => ref.getName.equalsIgnoreCase("ASIC")).get
    val enrichedLineASIC = referenceASIC.enrich(formatter)

    assert(enrichedLineASIC.contains("\\hyperref"), "ASIC not enriched")

    val referenceUniversalSerialBus = referencesWithActualReferences.find(ref => ref.getName.equalsIgnoreCase("Universal Serial Bus")).get
    val enrichedLineUniversalSerialBus = referenceUniversalSerialBus.enrich(formatter)

    assert(enrichedLineUniversalSerialBus.contains("\\hyperref"), "Universal Serial Bus not enriched with reference.")

    val differences = referencesWithReferences.diff(referencesWithActualReferences)
    differences.size should be(5)
  }
}






