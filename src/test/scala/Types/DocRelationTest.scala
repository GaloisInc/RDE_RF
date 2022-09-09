package Types

import Formatter.{InlineFormatter, ReferenceFormatter}
import Types.DocReference.DocReference
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should


class DocRelationTest extends AnyFlatSpec with should.Matchers {
  private val formatter: ReferenceFormatter = ReferenceFormatter(new InlineFormatter)

  "DocRelation" should "be able to be created" in {
    val documentName = "documentName"
    val relationReference = RelationReference("source", "target")
    val sourceReferenceName = ReferenceName("sourceRef")
    val targetReferenceName = ReferenceName("targetRef")
    val referenceType = ReferenceType.Requirement
    val documentType = DocumentType.Lando
    val originalLine = "originalLine"
    val sourceRef = DocReference(documentName, sourceReferenceName, referenceType, documentType, originalLine)
    val targetRef = DocReference(documentName, targetReferenceName, referenceType, documentType, originalLine)
    val relationType = RelationType.client
    val docRelation = DocRelation(
      documentName,
      relationReference,
      relationType,
      originalLine,
      Some(sourceRef),
      Some(targetRef),
    )

    docRelation should not be null
  }


  "DocRelation" should "test enriched String" in {
    val documentName = "documentName"
    val sourceReferenceName = ReferenceName("sourceRef")
    val targetReferenceName = ReferenceName("targetRef")
    val referenceType = ReferenceType.Requirement
    val documentType = DocumentType.Lando
    val originalLine = "originalLine"
    val sourceRef = DocReference(documentName, sourceReferenceName, referenceType, documentType, originalLine)
    val targetRef = DocReference(documentName, targetReferenceName, referenceType, documentType, originalLine)

    val docRelation = DocRelation(
      documentName,
      RelationReference(sourceReferenceName.name, targetReferenceName.name),
      RelationType.client,
      originalLine,
      Some(sourceRef),
      Some(targetRef),
    )

    val enrichedLine = docRelation.enrichedLine(formatter)
    enrichedLine should be("relation (*\\hyperref[documentName_Requirement_sourceRef]{sourceRef} (\\cref{documentName_Requirement_sourceRef})*) client (*\\hyperref[documentName_Requirement_targetRef]{targetRef} (\\cref{documentName_Requirement_targetRef})*)")
  }


}
