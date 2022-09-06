package Types

import Formatter.{InlineFormatter, ReferenceFormatter}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class DocReferenceTest extends AnyFlatSpec with should.Matchers {
  private val formatter: ReferenceFormatter = ReferenceFormatter(new InlineFormatter)

  "DocReference" should "be able to be created" in {
    val documentName = "documentName"
    val referenceName = ReferenceName("referenceName")
    val referenceType = ReferenceType.Requirement
    val documentType = DocumentType.Lando
    val originalLine = "originalLine"
    val docReference = DocReference(documentName, referenceName, referenceType, documentType, originalLine)
    docReference should not be null
  }

  "DocReference" should "test getters" in {
    val documentName = "documentName"
    val referenceName = ReferenceName("referenceName")
    val referenceType = ReferenceType.Requirement
    val documentType = DocumentType.Lando
    val originalLine = "originalLine"
    val docReference = DocReference(documentName, referenceName, referenceType, documentType, originalLine)

    docReference.getDocumentName should be(documentName)
    docReference.getReference should be(referenceName)
    docReference.getReferenceType should be(referenceType)
    docReference.getDocumentType should be(documentType)
    docReference.getOriginalLine should be(originalLine)
    docReference.getAcronym should be(None)
    docReference.getAbstracts should be(None)
    docReference.getSpecializes should be(None)
    docReference.getLabelText should be ("documentName_Requirement_referenceName")

    docReference.enrichedLine(formatter) should be("originalLine(*\\label{documentName_Requirement_referenceName}*)")
  }
}
