package Referencer

import DocumentEnrichers.{LandoDocumentEnricher, SysMLDocumentEnricher}
import Formatter.InlineFormatter
import Types.DocReference.DocReference
import Types.{DocumentType, ReferenceName, ReferenceType}
import Utils.{Control, FileUtil}
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import java.nio.file.Path
import scala.collection.mutable
import scala.io.Source

class ReferencerTest extends AnyFlatSpec with should.Matchers {
  private val formatterType = InlineFormatter()
  private val landoDocumentEnricher = LandoDocumentEnricher(formatterType)
  private val sysMLDocumentEnricher = SysMLDocumentEnricher(formatterType)

  private val sysMLReferencer = SysMLReferencer()

  "Reference" should "match coq with Reference" in {
    val searchReferenceName = "Coq"

    val referenceBeingDiscovered =
      DocReference("DocumentName",
        ReferenceName("Coq"),
        ReferenceType.Component,
        DocumentType.Lando,
        "component Coq"
      )
    val refinementBeingDiscovered =
      DocReference("DocumentName",
        ReferenceName("Coq"),
        ReferenceType.Component,
        DocumentType.SysML,
        "component Coq"
      )
    val result = sysMLReferencer.isSpecialization(referenceBeingDiscovered, refinementBeingDiscovered)
    assert(result)
  }

  "Referencer" should "match coq with Reference" in {
    val sysmlDocuments = getClass.getResource("../SysML").getPath
    val landoDocuments = getClass.getResource("../lando").getPath

    val landoFilesToAnalyse = FileUtil.getListOfFiles(landoDocuments).filter(_.contains("glossary"))
    val sysMLFilesToAnalyse = FileUtil.getListOfFiles(sysmlDocuments).filter(_.contains("Glossary"))

    val analysedLandoDocument = landoDocumentEnricher.extractDocumentInfo(landoFilesToAnalyse.head)
    val analysedSysMLDocument = sysMLDocumentEnricher.extractDocumentInfo(sysMLFilesToAnalyse.head)

    val coqReferenceName = analysedSysMLDocument.getAllReferences.filter(_.getName.equalsIgnoreCase("coq")).head

    assert(analysedLandoDocument.getAllReferences.exists(ref => sysMLReferencer.isSpecialization(coqReferenceName, ref)))
  }

  "Referencer" should "be able to match all References between modified glossary." in {
    val sysmlDocuments = getClass.getResource("../SysML_changed").getPath
    val landoDocuments = getClass.getResource("../lando_changed").getPath

    val landoFilesToAnalyse = FileUtil.getListOfFiles(landoDocuments).filter(_.contains("glossary"))
    val sysMLFilesToAnalyse = FileUtil.getListOfFiles(sysmlDocuments).filter(_.contains("Glossary"))

    val analysedLandoDocument = landoDocumentEnricher.extractDocumentInfo(landoFilesToAnalyse.head)
    val analysedSysMLDocument = sysMLDocumentEnricher.extractDocumentInfo(sysMLFilesToAnalyse.head)

    analysedSysMLDocument.getAllReferences.forall(
      sysMLRef => {
        assert(analysedLandoDocument.getAllReferences.exists(ref => sysMLReferencer.isSpecialization(sysMLRef, ref)), s"Could not find abstraction for ${sysMLRef.getName} in Lando")
        true
      }
    )
  }
}



