import DocumentEnrichers.SysMLDocumentEnricher
import Formatter.InlineFormatter
import TestUtils.TestUtility
import Types.*
import Utils.{Control, FileUtil}
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import java.io.File
import scala.None
import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex

class SysMLAnalyzerTest extends AnyFlatSpec with should.Matchers {
  private val formatterType = InlineFormatter()
  private val documentEnricher = SysMLDocumentEnricher(formatterType)
  private val expectedDocumentType = DocumentType.SysML
  private val resourceFolder = "../SysML"
  private val testUtility = TestUtility()


  "SysMLReader" should "to extract DocReference" in {
    val fileName = "PartsAndItems"
    val fileType = FileType.ComponentFile
    val line1 = "abstract item id BISL 'Behavioral Interface Specification Language';;"

    val extractedReference1 = documentEnricher.transformReference(line1, fileName, fileType)
    val expectedReference1 = DocReference(fileName, ReferenceName("Behavioral Interface Specification Language", Some("BISL")), ReferenceType.Component, DocumentType.SysML, line1)
    extractedReference1.getName should be(expectedReference1.getName)

    val line2 = "requirement 'Requirements Colloquial Completeness' : 'NRC Characteristic';"
    val expectedReference2 = DocReference(fileName, ReferenceName("Requirements Colloquial Completeness"), ReferenceType.Requirement, DocumentType.SysML, line2)
    val extractedReference2 = documentEnricher.transformReference(line2, fileName, fileType)

    extractedReference2.getName should be(expectedReference2.getName)

    val line3 = "package id Glossary 'Project Glossary' {"
    val expectedReference3 = DocReference(fileName, ReferenceName("Project Glossary", Some("Glossary")), ReferenceType.System, DocumentType.SysML, line3)
    val extractedReference3 = documentEnricher.transformReference(line3, fileName, fileType)
    extractedReference3.getName should be(expectedReference3.getName)


    val line4 = "abstract part def id SWImpl 'Hand-written Software Implementation'"
    val expectedReference4 = DocReference(fileName, ReferenceName("Hand-written Software Implementation", Some("SWImpl")), ReferenceType.Component, DocumentType.SysML, line4)
    val extractedReference4 = documentEnricher.transformReference(line4, fileName, fileType)
    extractedReference4.getName should be(expectedReference4.getName)


    val line5 = "part def SAWscript;"
    val expectedReference5 = DocReference(fileName, ReferenceName("SAWscript"), ReferenceType.Component, DocumentType.SysML, line5)
    val extractedReference5 = documentEnricher.transformReference(line5, fileName, fileType)
    extractedReference5.getName should be(expectedReference5.getName)

    val line6 = "item def 'RTS User';"
    val expectedReference6 = DocReference(fileName, ReferenceName("RTS User"), ReferenceType.Component, DocumentType.SysML, line6)
    val extractedReference6 = documentEnricher.transformReference(line6, fileName, fileType)
    extractedReference6.getName should be(expectedReference6.getName)
    extractedReference6.getReferenceType should be(expectedReference6.getReferenceType)

    val line7 = "abstract item def 'Consistent Model' :> Consistent, Model;"
    val expectedReference7 = DocReference(fileName, ReferenceName("Consistent Model"), ReferenceType.Component, DocumentType.SysML, line7)
    val extractedReference7 = documentEnricher.transformReference(line7, fileName, fileType)
    extractedReference7.getName should be(expectedReference7.getName)
    extractedReference7.getReferenceType should be(expectedReference7.getReferenceType)
  }


  "SysMLReader" should "to extract parts and items" in {
    val fileName = "RTS_Glossary"
    testUtility.checkExtractReferences("RTS_Glossary", documentEnricher, expectedDocumentType, resourceFolder, numberExprectedSystem = 1, numberOfSubSystem = 80, numberOfComponent = 66)
  }

  "SysMLReader" should "to extract requirements" in {
    val fileName = "RTS_Requirements"
    testUtility.checkExtractReferences(fileName, documentEnricher, expectedDocumentType, resourceFolder, 4, 0, 0, 17, 0, 0, 0, 0)
  }

  "SysMLReader" should "to extract packages" in {
    val fileName = "HARDENS"
    testUtility.checkExtractReferences(fileName, documentEnricher, expectedDocumentType, resourceFolder, 7, 0, 0, 0, 0, 0, 0, 0)
  }

  "SysMLReader" should "to extract Scenarios" in {
    val fileName = "RTS_Scenarios"
    testUtility.checkExtractReferences(fileName, documentEnricher, expectedDocumentType, resourceFolder, 4, 0, 1, numberOfScenarios = 39)
  }

  "SysMLReader" should "to extract Views" in {
    val fileName = "RTS_Viewpoints"
    testUtility.checkExtractReferences(fileName, documentEnricher, expectedDocumentType, resourceFolder, numberExprectedSystem = 1, numberOfViews = 8)
  }

  "SysMLReader" should "to extract Actions" in {
    val fileName = "RTS_Actions"
    testUtility.checkExtractReferences(fileName, documentEnricher, expectedDocumentType, resourceFolder, numberExprectedSystem = 4, numberOfEvents = 20)
  }

}






