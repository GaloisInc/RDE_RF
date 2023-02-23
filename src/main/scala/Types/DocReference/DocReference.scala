package Types.DocReference

import Formatter.{LatexSanitizer, ReferenceFormatter}
import Types.Reference.Ref
import Types._

class DocReference(
                    override val documentName: String,
                    referenceName: ReferenceName,
                    referenceType: ReferenceType.Value,
                    documentType: DocumentType.Value,
                    override val originalLine: String,
                    refinementOf: Option[Set[DocReference]] = None,
                    abstractionOf: Option[Set[DocReference]] = None,
                    references: Option[Set[Ref]] = None,
                  ) extends DecorateableString with DocumentReference {
  def addAbstraction(abstraction: DocReference): DocReference = {
    getAbstractions match {
      case Some(abstractions) => this.copy(refinementOf = Some(abstractions + abstraction))
      case None => this.copy(refinementOf = Some(Set(abstraction)))
    }
  }

  def addRefinement(refinement: DocReference): DocReference = {
    getRefinements match {
      case Some(refinements) => this.copy(abstractionOf = Some(refinements + refinement))
      case None => this.copy(abstractionOf = Some(Set(refinement)))
    }
  }

  require(originalLine.nonEmpty, "originalLine must not be empty")
  require(documentName.nonEmpty, "documentName must not be empty")
  require(getName.nonEmpty, "" +
    "referenceName must not be empty (documentName: " + documentName + ") and line: " + originalLine)

  lazy val getLabelText: String = s"${documentType.toString}_${documentName}_${getReferenceType.toString}_${LatexSanitizer.sanitizeReferenceName(getName)}"

  lazy val getName: String = if (referenceName.name.isEmpty) referenceName.acronym.get else referenceName.name

  lazy val shortName: String = if (referenceName.acronym.isDefined && referenceName.acronym.get.nonEmpty) referenceName.acronym.get else referenceName.name

  lazy val sanitizedName: String = LatexSanitizer.sanitizeName(getName)

  def updateDocReference(ref: DocReference): DocReference =
    if (ref.originalLine == originalLine) ref else this

  //A reference can be a refinement or an abstraction of another reference
  private var referencing: Map[String, DocReference] = Map.empty

  def getStringReferences: Option[Set[Ref]] = references

  def isReferencingAnything: Boolean = references.isDefined && references.nonEmpty

  def addReference(ref: (String, DocReference)): Unit = {
    require(ref != null, "ref must not be null")
    require(ref._2.getDocumentType == getDocumentType, "ref must have the same document type to be referenced!")
    require(!referencing.contains(ref._1), "ref must not be already referenced!")
    require(references.get.exists(r => r.getCleanName.equalsIgnoreCase(ref._1)), "ref must be in the references of this reference! But was: " + ref._1)
    referencing = referencing + ref
  } ensuring(referencing.contains(ref._1) &&
    referencing.nonEmpty &&
    referencing.size <= references.get.size, "ref must be added to the referencing references for ref " + ref._1)

  def getReferences: Map[String, DocReference] = referencing

  def getAcronym: Option[String] = referenceName.acronym

  def getDocumentName: String = documentName

  def getReferenceType: ReferenceType.Value = referenceType

  def getDocumentType: DocumentType.Value = documentType

  def getReferenceName: ReferenceName = referenceName

  def getOriginalLine: String = originalLine

  def getAbstractions: Option[Set[DocReference]] = refinementOf

  def getRefinements: Option[Set[DocReference]] = abstractionOf

  def copy(
            documentName: String = documentName,
            referenceName: ReferenceName = referenceName,
            referenceType: ReferenceType.Value = referenceType,
            documentType: DocumentType.Value = documentType,
            originalLine: String = originalLine,
            refinementOf: Option[Set[DocReference]] = refinementOf,
            abstractionOf: Option[Set[DocReference]] = abstractionOf,
            references: Option[Set[Ref]] = references,
          ): DocReference = new DocReference(
    documentName,
    referenceName,
    referenceType,
    documentType,
    originalLine,
    refinementOf,
    abstractionOf,
    references,
  )

  def isInRefinementChain: Boolean =
    (abstractionOf.isDefined && abstractionOf.nonEmpty) || (refinementOf.isDefined && refinementOf.nonEmpty)

  override def enrich(formatter: ReferenceFormatter): String = {
    val refinementOfString = getAbstractions match {
      case Some(abstracts) => formatter.addAbstractions(abstracts, documentName)
      case None => ""
    }

    val abstractionOfString = getRefinements match {
      case Some(specializes) => formatter.addSpecializations(specializes, documentName)
      case None => ""
    }

    //If the original line contains a reference to another reference, we highlight it by making a clickable link
    val highlightedLine = references match {
      case Some(reference) => formatter.highlightLineWithReferences(originalLine, reference.head.symbol, getReferences)
      case None => originalLine
    }

    val lineWithLabel = formatter.enrichLineWithLabel(highlightedLine, getLabelText)
    if (isInRefinementChain) {
      lineWithLabel + refinementOfString + abstractionOfString
    } else {
      lineWithLabel
    }
  }
}




