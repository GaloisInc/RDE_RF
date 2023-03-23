package Types.DocReference

trait DocumentReference {
  def documentName: String
}
/*
sealed trait DocReferenceType extends DocumentReference with DecorateableString {
  def name: String

  def originalLine: String

  def documentName: String

  def documentType: Types.DocumentType.Value

  // Line number of the reference in the document
  def lineNumber: Int


  def label: String = {
    s"${documentType.toString}_${documentName}_${getReferenceType.toString}_${LatexSanitizer.sanitizeReferenceName(name)}"
  }


  //A reference can be a refinement or an abstraction of another reference
  def refinements: Option[Set[DocReference]]

  //A reference can be a refinement or an abstraction of another reference
  def abstractions: Option[Set[DocReference]]

  // A reference can have a relation to another reference in the same document
  def relations: Option[Set[DocReference]]


  def isInRefinementChain: Boolean =
    (abstractions.isDefined && abstractions.nonEmpty) || (refinements.isDefined && refinements.nonEmpty)

  require(originalLine.nonEmpty, "originalLine must not be empty")
  require(documentName.nonEmpty, "documentName must not be empty")

  override def enrich(formatter: ReferenceFormatter): String = {
    def stringOfReference(references: Option[Set[DocReference]], formatting: (Set[DocReference], String) => String): String =
      references match {
        case Some(abstracts) => formatting(abstracts, documentName)
        case None => ""
      }

    val abstractionOfString = stringOfReference(refinements, formatter.addSpecializations)
    val refinementOfString = stringOfReference(abstractions, formatter.addAbstractions)

    //If the original line contains a reference to another reference, we highlight it by making a clickable link
    val highlightedLine = relations match {
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

final case class LandoDocReference(
                                    referenceName: ReferenceName,
                                    override val documentName: String,
                                    override val originalLine: String,
                                    override val lineNumber: Int,
                                    override val refinements: Option[Set[DocReference]] = None,
                                    override val relations: Option[Set[DocReference]] = None,
                                  ) extends DocReferenceType {
  val documentType: Types.DocumentType.Value = Types.DocumentType.Lando

  // Lando can not have abstractions
  val abstractions: Option[Set[DocReference]] = None

  override def name: String = referenceName.name
}

final case class LobotReference(
                                 referenceName: ReferenceName,
                                 override val documentName: String,
                                 override val originalLine: String,
                                 override val lineNumber: Int,
                                 override val refinements: Option[Set[DocReference]] = None,
                                 override val abstractions: Option[Set[DocReference]] = None,
                                 override val relations: Option[Set[DocReference]] = None,
                               ) extends DocReferenceType {
  val documentType: Types.DocumentType.Value = Types.DocumentType.Lobot

  override def name: String = referenceName.name
}

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
*/