package Types

import Formatter.{LatexSanitizer, LatexSyntax, ReferenceFormatter}

class DocRelation(
                   override val documentName: String,
                   relationReference: RelationReference,
                   relationType: RelationType,
                   override val originalLine: String,
                   sourceRef: Option[DocReference],
                   targetRef: Option[DocReference],
                 ) extends EnrichableString {

  require(originalLine.nonEmpty, "originalLine must not be empty")
  require(documentName.nonEmpty, "documentName must not be empty")

  def getDocumentName: String = documentName

  def getOriginalLine: String = originalLine

  def getRelationReference: RelationReference = relationReference

  def getRelationType: RelationType = relationType

  def getSourceName: String = relationReference.sourceName

  def getTargetName: String = relationReference.targetName

  def getSourceRef: Option[DocReference] = sourceRef

  def getTargetRef: Option[DocReference] = targetRef

  override def enrichedLine(formatter: ReferenceFormatter): String = {
    require(sourceRef.isDefined, "sourceRef must be defined")
    require(targetRef.isDefined, "targetRef must be defined")
    val linkToSource = LatexSyntax.addClickableLocalLink(sourceRef.get.getLabelText, sourceRef.get.getName)
    val linkToTarget = LatexSyntax.addClickableLocalLink(targetRef.get.getLabelText, targetRef.get.getName)

    s"relation $linkToSource $relationType.toString $linkToTarget"
  }

  def getName: String = {
    s"${documentName}_${relationType.toString}_${LatexSanitizer.sanitizeReferenceName(getSourceName)}_${LatexSanitizer.sanitizeReferenceName(getTargetName)}"
  }


}
