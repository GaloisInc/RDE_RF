package Types

import Formatter.{LatexSanitizer, ReferenceFormatter}
import Types.DocReference.{DocReference, DocumentReference}


class DocRelation(
                   override val documentName: String,
                   referenceRelation: RelationReference,
                   relationType: RelationTypes.Value,
                   override val originalLine: String,
                   sourceRef: Option[DocReference],
                   targetRef: Option[DocReference],
                 ) extends DecorateableString with DocumentReference {

  require(originalLine.nonEmpty, "originalLine must not be empty")
  require(documentName.nonEmpty, "documentName must not be empty")

  def getSourceName: String = referenceRelation.sourceName

  def getTargetName: String = referenceRelation.targetName

  def copy(
            documentName: String = documentName,
            referenceRelation: RelationReference = referenceRelation,
            relationType: RelationTypes.relationType = relationType,
            originalLine: String = originalLine,
            sourceRef: Option[DocReference] = sourceRef,
            targetRef: Option[DocReference] = targetRef,
          ): DocRelation = {
    new DocRelation(documentName, referenceRelation, relationType, originalLine, sourceRef, targetRef)
  }

  def getDocumentName: String = documentName

  def getOriginalLine: String = originalLine


  def getRelationType: RelationTypes.relationType = relationType

  override def enrich(formatter: ReferenceFormatter): String = {
    val linkToSource = if (sourceRef.isDefined) formatter.addReference(sourceRef.get, documentName, LatexReferenceTypes.ConnectionArtifact) else getSourceName
    val linkToTarget = if (targetRef.isDefined) formatter.addReference(targetRef.get, documentName, LatexReferenceTypes.ConnectionArtifact) else getTargetName

    s"relation $linkToSource ${relationType.toString} $linkToTarget"
  }

  def getName: String = {
    s"${documentName}_${relationType.toString}_${LatexSanitizer.sanitizeReferenceName(getSourceName)}_${LatexSanitizer.sanitizeReferenceName(getTargetName)}"
  }
}
