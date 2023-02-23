package Types

import Formatter.{InlineFormatter, ReferenceFormatter}
import Types.DocReference.DocReference
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class DocReferenceTest extends AnyFlatSpec with should.Matchers {
  private val formatter: ReferenceFormatter = new ReferenceFormatter(new InlineFormatter)

  "DocReference" should "be able to be created" in {
    val documentName = "documentName"
    val referenceName = ReferenceName("referenceName")
    val referenceType = ReferenceType.Requirement
    val documentType = DocumentType.Lando
    val originalLine = "originalLine"
    val docReference = new DocReference(documentName, referenceName, referenceType, documentType, originalLine)
    docReference should not be null
  }

  "DocReference" should "test getters" in {
    val documentName = "documentName"
    val referenceName = ReferenceName("referenceName")
    val referenceType = ReferenceType.Requirement
    val documentType = DocumentType.Lando
    val originalLine = "originalLine"
    val docReference = new DocReference(documentName, referenceName, referenceType, documentType, originalLine)

    docReference.getDocumentName should be(documentName)
    docReference.getReferenceName should be(referenceName)
    docReference.getReferenceType should be(referenceType)
    docReference.getDocumentType should be(documentType)
    docReference.getOriginalLine should be(originalLine)
    docReference.getAcronym should be(None)
    docReference.getAbstractions should be(None)
    docReference.getRefinements should be(None)
    docReference.getLabelText should be("Lando_documentName_Requirement_referenceName")

    docReference.enrich(formatter) should be("originalLine(@\\label{Lando_documentName_Requirement_referenceName}@)")
  }


  "DocReference" should "test abstractions" in {
    val documentName = "documentName"
    val referenceName = ReferenceName("referenceName")
    val referenceType = ReferenceType.Requirement
    val documentType = DocumentType.Lando
    val originalLine = "originalLine"

    val referenceNameOfAbstraction = ReferenceName("abstractName")
    val abstraction = new DocReference(documentName, referenceNameOfAbstraction, referenceType, documentType, originalLine)
    val docReference = new DocReference(documentName, referenceName, referenceType, documentType, originalLine, refinementOf = Some(Set(abstraction)))
    docReference.getDocumentName should be(documentName)
    docReference.getReferenceName should be(referenceName)
    docReference.getReferenceType should be(referenceType)
    docReference.getDocumentType should be(documentType)
    docReference.getOriginalLine should be(originalLine)
    docReference.getAcronym should be(None)
    docReference.getAbstractions should be(Some(Set(abstraction)))
    docReference.getRefinements should be(None)
    docReference.getLabelText should be("Lando_documentName_Requirement_referenceName")

    docReference.enrich(formatter) should be(s"originalLine(@\\label{${docReference.getLabelText}}@)(@($$\\sqsupseteq$$\\abstractionLink{${abstraction.getLabelText}}{${abstraction.getName}} \\cref{${abstraction.getLabelText}})@)")
  }
}
