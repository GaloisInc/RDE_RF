package Types.DocReference

import Formatter.{LatexSanitizer, ReferenceFormatter}
import Types.Reference.Ref
import Types.*

class DocReference(
                    override val documentName: String,
                    referenceName: ReferenceName,
                    referenceType: ReferenceType,
                    documentType: DocumentType,
                    override val originalLine: String,
                    refinementOf: Option[Set[DocReference]] = None,
                    abstractionOf: Option[Set[DocReference]] = None,
                    references: Option[Set[Ref]] = None,
                  ) extends EnrichableString with DocumentReference {

  require(originalLine.nonEmpty, "originalLine must not be empty")
  require(documentName.nonEmpty, "documentName must not be empty")
  require(getName.nonEmpty, "" +
    "referenceName must not be empty (documentName: " + documentName + ") and line: " + originalLine)

  lazy val getLabelText: String = s"${documentName}_${getReferenceType.toString}_${LatexSanitizer.sanitizeReferenceName(getName)}"

  lazy val getName: String = if referenceName.name.isEmpty then referenceName.acronym.get else referenceName.name

  lazy val getShortName: String = if referenceName.acronym.isDefined && referenceName.acronym.nonEmpty then referenceName.acronym.get else referenceName.name

  lazy val sanitizedName: String = LatexSanitizer.sanitizeName(getName)

  def getAcronym: Option[String] = referenceName.acronym

  def getDocumentName: String = documentName

  def getReferenceType: ReferenceType = referenceType

  def getDocumentType: DocumentType = documentType

  def getReference: ReferenceName = referenceName

  def getOriginalLine: String = originalLine

  def getAbstractions: Option[Set[DocReference]] = refinementOf

  def getRefinements: Option[Set[DocReference]] = abstractionOf

  def isInRefinementChain: Boolean = (abstractionOf.isDefined && abstractionOf.nonEmpty) || (refinementOf.isDefined && refinementOf.nonEmpty)

  override def enrichedLine(formatter: ReferenceFormatter): String = {
    val refinementOfString = getAbstractions match {
      case Some(abstracts) => formatter.addAbstractions(abstracts, documentName)
      case None => ""
    }

    val abstractionOfString = getRefinements match {
      case Some(specializes) => formatter.addSpecializations(specializes, documentName)
      case None => ""
    }

    val lineWithLabel = formatter.enrichLineWithLabel(originalLine, getLabelText)
    if (isInRefinementChain) {
      lineWithLabel + refinementOfString + abstractionOfString
    } else {
      lineWithLabel
    }
  }
}


class EnrichedDocReference(
                            override val documentName: String,
                            referenceName: ReferenceName,
                            referenceType: ReferenceType,
                            documentType: DocumentType,
                            override val originalLine: String,
                            refinementOf: Option[Set[DocReference]] = None,
                            abstractionOf: Option[Set[DocReference]] = None,
                            references: Option[Set[DocReference]] = None,
                          ) extends EnrichableString with DocumentReference {

  require(originalLine.nonEmpty, "originalLine must not be empty")
  require(documentName.nonEmpty, "documentName must not be empty")
  require(getName.nonEmpty, "" + "referenceName must not be empty (documentName: " + documentName + ") and line: " + originalLine)


  lazy val getLabelText: String = s"${documentName}_${getReferenceType.toString}_${LatexSanitizer.sanitizeReferenceName(getName)}"

  lazy val getName: String = if referenceName.name.isEmpty then referenceName.acronym.get else referenceName.name

  lazy val sanitizedName: String = LatexSanitizer.sanitizeName(getName)

  def getAcronym: Option[String] = referenceName.acronym

  def getDocumentName: String = documentName

  def getReferenceType: ReferenceType = referenceType

  def getDocumentType: DocumentType = documentType

  def getReference: ReferenceName = referenceName

  def getOriginalLine: String = originalLine

  def getAbstractions: Option[Set[DocReference]] = refinementOf

  def getRefinements: Option[Set[DocReference]] = abstractionOf

  def isInRefinementChain: Boolean = (abstractionOf.isDefined && abstractionOf.nonEmpty) || (refinementOf.isDefined && refinementOf.nonEmpty)

  override def enrichedLine(formatter: ReferenceFormatter): String = {
    val refinementOfString = getAbstractions match {
      case Some(abstracts) => formatter.addAbstractions(abstracts, documentName)
      case None => ""
    }

    val abstractionOfString = getRefinements match {
      case Some(specializes) => formatter.addSpecializations(specializes, documentName)
      case None => ""
    }

    val highlightedLine = formatter.highlightLineWithReferences(originalLine, references)

    val lineWithLabel = formatter.enrichLineWithLabel(highlightedLine, getLabelText)
    if (isInRefinementChain) {
      lineWithLabel + refinementOfString + abstractionOfString
    } else {
      lineWithLabel
    }
  }
}




