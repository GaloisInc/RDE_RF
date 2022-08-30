//import Utils.Control
//import org.scalatest.*
//import flatspec.*
//import matchers.*
//import Types.*
//
//import java.io.File
//import scala.io.Source
//import scala.collection.mutable
//
//class DocumentAnalyzerTest extends AnyFlatSpec with should.Matchers {
//  "DocumentReader" should "be able to detect acronyms" in {
//    val acronymText1 = "component Core Finite State Machine (CFSM)"
//    val acronymText2 = "component Programming I/O (Programming_IO)"
//    val acronymText3 = "component Cryptol Software Compiler (CryptolToC)"
//    val acronymText4 = "component Cryptol Software Compiler"
//
//    val expectedAcronym1 = "CFSM"
//    val expectedAcronym2 = "Programming_IO"
//    val expectedAcronym3 = "CryptolToC"
//
//    val expectedName1 = "Core Finite State Machine"
//    val expectedName2 = "Programming I/O"
//    val expectedName3 = "Cryptol Software Compiler"
//
//
//    val referenceName1 = LandoDocumentReader.extractReferenceName(acronymText1, LandoReferenceType.Component)
//    val referenceName2 = LandoDocumentReader.extractReferenceName(acronymText2, LandoReferenceType.Component)
//    val referenceName3 = LandoDocumentReader.extractReferenceName(acronymText3, LandoReferenceType.Component)
//    val referenceName4 = LandoDocumentReader.extractReferenceName(acronymText4, LandoReferenceType.Component)
//
//    assert(expectedAcronym1 == referenceName1.acronym.get)
//    assert(expectedAcronym2 == referenceName2.acronym.get)
//    assert(expectedAcronym3 == referenceName3.acronym.get)
//    assert(referenceName4.acronym.isEmpty)
//
//    // Assertions on name
//    assert(expectedName1 == referenceName1.name)
//    assert(expectedName2 == referenceName2.name)
//    assert(expectedName3 == referenceName3.name)
//    assert(expectedName3 == referenceName4.name)
//  }
//
//  "DocumentReader" should "be able detect reference type" in {
//    val text1 = "component Core Finite State Machine (CFSM)"
//    val text2 = "subsystem RTS Implementation Artifacts (Artifacts)"
//    val text3 = "system Cryptol Software Compiler (CryptolToC)"
//    val text4 = "Cryptol Software Compiler"
//
//    val expectedRef1 = LandoReferenceType.Component
//    val expectedRef2 = LandoReferenceType.SubSystem
//    val expectedRef3 = LandoReferenceType.System
//    val expectedRef4 = None
//
//    val LandoReferenceType1 = LandoDocumentReader.getReferenceType(text1, FileType.ComponentFile)
//    val LandoReferenceType2 = LandoDocumentReader.getReferenceType(text2, FileType.ComponentFile)
//    val LandoReferenceType3 = LandoDocumentReader.getReferenceType(text3, FileType.ComponentFile)
//    val LandoReferenceType4 = LandoDocumentReader.getReferenceType(text4, FileType.ComponentFile)
//
//    assert(expectedRef1 == LandoReferenceType1.get)
//    assert(expectedRef2 == LandoReferenceType2.get)
//    assert(expectedRef3 == LandoReferenceType3.get)
//    assert(LandoReferenceType4.isEmpty)
//    assert(expectedRef4 == LandoReferenceType4)
//  }
//
//
//  "DocumentReader" should "be able detect relation types" in {
//    val text1 = "relation Board client Debug-C"
//    val text2 = "relation Debug-C client Developer Machine"
//    val text3 = "relation FPGA Dev Board contains J2"
//    val text4 = "relation Temperature Sensor inherit Sensor"
//
//    val expectedRef1Type = LandoRelationType.client
//    val expectedRef2Type = LandoRelationType.client
//    val expectedRef3Type = LandoRelationType.contains
//    val expectedRef4Type = LandoRelationType.inherit
//
//    val relation1 = LandoDocumentReader.getRelationType(text1)
//    val relation2 = LandoDocumentReader.getRelationType(text2)
//    val relation3 = LandoDocumentReader.getRelationType(text3)
//    val relation4 = LandoDocumentReader.getRelationType(text4)
//
//    val expectedRef1Src = "Board"
//    val expectedRef2Src = "Debug-C"
//    val expectedRef3Src = "FPGA Dev Board"
//    val expectedRef4Src = "Temperature Sensor"
//
//    val expectedRef1Trg = "Debug-C"
//    val expectedRef2Trg = "Developer Client"
//    val expectedRef3Trg = "J2"
//    val expectedRef4Trg = "Sensor"
//
//    assert(expectedRef1Type == relation1.get)
//    assert(expectedRef2Type == relation2.get)
//    assert(expectedRef3Type == relation3.get)
//    assert(expectedRef4Type == relation4.get)
//  }
//
//
//  "DocumentReader" should "be able detect relation" in {
//    val text1 = "relation Board client Debug-C"
//    val text2 = "relation Debug-C client Developer Machine"
//    val text3 = "relation FPGA Dev Board contains J2"
//    val text4 = "relation Temperature Sensor inherit Sensor"
//
//    val LandoRelationType1 = LandoDocumentReader.getRelationType(text1)
//    val LandoRelationType2 = LandoDocumentReader.getRelationType(text2)
//    val LandoRelationType3 = LandoDocumentReader.getRelationType(text3)
//    val LandoRelationType4 = LandoDocumentReader.getRelationType(text4)
//
//    val relation1 = LandoDocumentReader.extractRelationName(text1, LandoRelationType1.get)
//    val relation2 = LandoDocumentReader.extractRelationName(text2, LandoRelationType2.get)
//    val relation3 = LandoDocumentReader.extractRelationName(text3, LandoRelationType3.get)
//    val relation4 = LandoDocumentReader.extractRelationName(text4, LandoRelationType4.get)
//
//    val expectedRef1Src = "Board"
//    val expectedRef2Src = "Debug-C"
//    val expectedRef3Src = "FPGA Dev Board"
//    val expectedRef4Src = "Temperature Sensor"
//
//    val expectedRef1Trg = "Debug-C"
//    val expectedRef2Trg = "Developer Machine"
//    val expectedRef3Trg = "J2"
//    val expectedRef4Trg = "Sensor"
//
//    assert(expectedRef1Src == relation1.sourceName)
//    assert(expectedRef2Src == relation2.sourceName)
//    assert(expectedRef3Src == relation3.sourceName)
//    assert(expectedRef4Src == relation4.sourceName)
//
//    assert(expectedRef1Trg == relation1.targetName)
//    assert(expectedRef2Trg == relation2.targetName)
//    assert(expectedRef3Trg == relation3.targetName)
//    assert(expectedRef4Trg == relation4.targetName)
//  }
//
//
//  "DocumentReader" should "be able to extract references" in {
//    val source = getClass.getResource("landoFile.lando").getPath
//    val references = LandoDocumentReader.extractReferences(source)
//
//    assert(references.nonEmpty)
//    assert(references.size == 66)
//    assert(references.count(i => i.referenceType == LandoReferenceType.Component) == 65)
//    assert(references.count(i => i.referenceType == LandoReferenceType.SubSystem) == 1)
//    assert(references.count(i => i.referenceType == LandoReferenceType.System) == 0)
//
//    val relations = LandoDocumentReader.extractRelations(source)
//
//    assert(relations.isEmpty)
//  }
//
//
//  "DocumentReader" should "be able to extract references and relations" in {
//    val source = getClass.getResource("landoFileWithRelations.lando").getPath
//    val references = LandoDocumentReader.extractReferences(source)
//
//    assert(references.nonEmpty)
//    assert(references.size == 35)
//    assert(references.count(i => i.referenceType == LandoReferenceType.Component) == 33)
//    assert(references.count(i => i.referenceType == LandoReferenceType.SubSystem) == 2)
//    assert(references.count(i => i.referenceType == LandoReferenceType.System) == 0)
//
//    val relations = LandoDocumentReader.extractRelations(source)
//
//    assert(relations.nonEmpty)
//    assert(relations.size == 22)
//    assert(relations.count(i => i.relationType == LandoRelationType.client) == 14)
//    assert(relations.count(i => i.relationType == LandoRelationType.inherit) == 6)
//    assert(relations.count(i => i.relationType == LandoRelationType.contains) == 2)
//
//  }
//
//  def getListOfFiles(dir: String): List[String] = {
//    val d = new File(dir)
//    if (d.exists && d.isDirectory) {
//      val filesToDelete = d.listFiles.filter(file => file.getName.contains("decorated"))
//      filesToDelete.foreach(file => file.delete())
//      d.listFiles.filter(_.isFile).map(_.toString).toList
//    } else {
//      List[String]()
//    }
//  }
//
//  "DocumentReader" should "to enrich references" in {
//    val source = getClass.getResource("landoFileWithRelations.lando").getPath
//    val documentInfo = LandoDocumentReader.analyseDocument(source)
//    LandoDocumentEnricher.enrichDocument(source, documentInfo)
//  }
//
//  "DocumentReader" should "to enrich references across documents" in {
//    val directory = getClass.getResource("lando").getPath
//
//    val filesToAnalyse = getListOfFiles(directory)
//
//    val documentsToAnalyse = filesToAnalyse.toArray
//    DocumentAnalyzer.enrichLandoDocuments(documentsToAnalyse)
//  }
//}
//
//
//
