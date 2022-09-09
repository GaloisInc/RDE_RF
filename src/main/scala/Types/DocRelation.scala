package Types

import Formatter.{LatexSanitizer, LatexSyntax, ReferenceFormatter}
import Types.DocReference.DocReference


class DocRelation(
                   override val documentName: String,
                   referenceRelation: RelationReference,
                   relationType: RelationType,
                   override val originalLine: String,
                   sourceRef: Option[DocReference],
                   targetRef: Option[DocReference],
                 ) extends EnrichableString with DocumentReference {

  require(originalLine.nonEmpty, "originalLine must not be empty")
  require(documentName.nonEmpty, "documentName must not be empty")

  def getSourceName: String = referenceRelation.sourceName

  def getTargetName: String = referenceRelation.targetName

  def copy(
            documentName: String = documentName,
            referenceRelation: RelationReference = referenceRelation,
            relationType: RelationType = relationType,
            originalLine: String = originalLine,
            sourceRef: Option[DocReference] = sourceRef,
            targetRef: Option[DocReference] = targetRef,
          ): DocRelation = {
    new DocRelation(documentName, referenceRelation, relationType, originalLine, sourceRef, targetRef)
  }

  def getDocumentName: String = documentName

  def getOriginalLine: String = originalLine


  def getRelationType: RelationType = relationType


  def getSourceRef: Option[DocReference] = sourceRef

  def getTargetRef: Option[DocReference] = targetRef

  override def enrichedLine(formatter: ReferenceFormatter): String = {
    val linkToSource = if sourceRef.isDefined then formatter.addReference(sourceRef.get, documentName, LatexReferenceType.ConnectionArtifact) else getSourceName
    val linkToTarget = if targetRef.isDefined then formatter.addReference(targetRef.get, documentName, LatexReferenceType.ConnectionArtifact) else getTargetName

    s"relation $linkToSource ${relationType.toString} $linkToTarget"
  }

  def getName: String = {
    s"${documentName}_${relationType.toString}_${LatexSanitizer.sanitizeReferenceName(getSourceName)}_${LatexSanitizer.sanitizeReferenceName(getTargetName)}"
  }
}
