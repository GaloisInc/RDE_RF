package Types

import Formatter.{LatexSanitizer, ReferenceFormatter}

class DocReference(
                    override val documentName: String,
                    referenceName: ReferenceName,
                    referenceType: ReferenceType,
                    documentType: DocumentType,
                    override val originalLine: String,
                    refinementOf: Option[Set[DocReference]] = None,
                    abstractionOf: Option[Set[DocReference]] = None,
                  ) extends EnrichableString {

  require(originalLine.nonEmpty, "originalLine must not be empty")
  require(documentName.nonEmpty, "documentName must not be empty")

  lazy val getLabelText: String = {
    s"${documentName}_${getReferenceType.toString}_${LatexSanitizer.sanitizeReferenceName(referenceName.name)}"
  }

  lazy val getName: String = referenceName.name

  def getAcronym: Option[String] = referenceName.acronym

  def getDocumentName: String = documentName

  def getReferenceType: ReferenceType = referenceType

  def getDocumentType: DocumentType = documentType

  def getReference: ReferenceName = referenceName

  def getOriginalLine: String = originalLine

  def getAbstractions: Option[Set[DocReference]] = refinementOf

  def getRefinements: Option[Set[DocReference]] = abstractionOf

  override def enrichedLine(formatter: ReferenceFormatter): String = {
    val refinementOfString = getAbstractions match {
      case Some(abstracts) => formatter.addAbstractions(abstracts, documentName)
      case None => ""
    }

    val abstractionOfString = getRefinements match {
      case Some(specializes) => formatter.addSpecializations(specializes, documentName)
      case None => ""
    }

    formatter.enrichLineWithLabel(originalLine, getLabelText) +
      refinementOfString +
      abstractionOfString
  }
}




