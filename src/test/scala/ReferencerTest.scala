import Referencer.SysMLReferencer
import Types.{DocReference, DocumentType, ReferenceName, ReferenceType}
import DocumentEnrichers.{LandoDocumentEnricher, SysMLDocumentEnricher}
import Utils.{Control, FileUtil}
import Formatter.InlineFormatter
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import java.nio.file.Path
import scala.collection.mutable
import scala.io.Source

class ReferencerTest extends AnyFlatSpec with should.Matchers {
  private val fileUtil = FileUtil()
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
    val result = sysMLReferencer.isSpecialization(searchReferenceName, referenceBeingDiscovered)
    assert(result)
  }

  "Referencer" should "match coq with Reference" in {
    val sysmlDocuments = getClass.getResource("SysML").getPath
    val landoDocuments = getClass.getResource("Lando").getPath

    val landoFilesToAnalyse = fileUtil.getListOfFiles(landoDocuments).filter(_.contains("glossary"))
    val sysMLFilesToAnalyse = fileUtil.getListOfFiles(sysmlDocuments).filter(_.contains("Glossary"))

    val analysedLandoDocument = landoDocumentEnricher.extractDocumentInfo(landoFilesToAnalyse.head)
    val analysedSysMLDocument = sysMLDocumentEnricher.extractDocumentInfo(sysMLFilesToAnalyse.head)

    val coqReferenceName = analysedSysMLDocument.getAllReferences.filter(_.getName.equalsIgnoreCase("coq")).head

    assert(analysedLandoDocument.getAllReferences.exists(ref => sysMLReferencer.isSpecialization(coqReferenceName.getName, ref)))
  }

  "Referencer" should "be able to match all SysML References" in {
    val sysmlDocuments = getClass.getResource("SysML").getPath
    val landoDocuments = getClass.getResource("Lando").getPath

    val landoFilesToAnalyse = fileUtil.getListOfFiles(landoDocuments).filter(_.contains("glossary"))
    val sysMLFilesToAnalyse = fileUtil.getListOfFiles(sysmlDocuments).filter(_.contains("Glossary"))

    val analysedLandoDocument = landoDocumentEnricher.extractDocumentInfo(landoFilesToAnalyse.head)
    val analysedSysMLDocument = sysMLDocumentEnricher.extractDocumentInfo(sysMLFilesToAnalyse.head)

    analysedSysMLDocument.getAllReferences.forall(
      sysMLRef => {
        assert(analysedLandoDocument.getAllReferences.exists(ref => sysMLReferencer.isSpecialization(sysMLRef.getReference.name, ref)))
        true
      }
    )
  }
}



