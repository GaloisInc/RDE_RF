package Referencer

import DocumentEnrichers.LatexFormatter
import Types.*

abstract class Referencer() {
  protected val latexFormatter = new LatexFormatter()

  def addSpecializationAndAbstract(documentToExtend: DocumentInfo, abstractDocuments: Array[DocumentInfo], specializedDocuments: Array[DocumentInfo]): DocumentInfo

  def addSpecializationsToDocument(abstractDocument: DocumentInfo, specializedDocuments: Array[DocumentInfo]): DocumentInfo

  def addAbstractionsToDocument(specializedDocument: DocumentInfo, abstractDocuments: Array[DocumentInfo]): DocumentInfo

  protected def addAbstractions(reference: DocReference, abstractReferences: Set[DocReference]): DocReference = {
    val referenceName = reference.referenceName.name
    val abstractions = abstractReferences.filter(isSpecialization(referenceName, _))
    if (abstractions.nonEmpty)
      enrichSpecializationWithAbstraction(reference, abstractions)
    else
      reference
  } ensuring ((ref: DocReference) =>
    ref.referenceType == reference.referenceType
      && ref.documentName == reference.documentName
      && ref.documentType == reference.documentType
      && ref.referenceName == reference.referenceName
      && ref.referenceType == reference.referenceType
      && ref.abstracts == reference.abstracts)

  protected def addSpecializations(reference: DocReference, specializedReferences: Set[DocReference]): DocReference = {
    require(reference.specializes.isEmpty)
    val specializations = specializedReferences.filter(ref => ref.specializes.isDefined && ref.specializes.get.contains(reference))
    if (specializations.nonEmpty)
      enrichAbstractionWithSpecialization(reference, specializations)
    else
      reference
  } ensuring ((ref: DocReference) =>
    ref.referenceType == reference.referenceType
      && ref.documentName == reference.documentName
      && ref.documentType == reference.documentType
      && ref.referenceName == reference.referenceName
      && ref.referenceType == reference.referenceType
      && ref.specializes == reference.specializes)

  private def isSpecialization(referenceName: String, ref: DocReference) = {
    val referenceNameToMatch = ref.referenceName.name.toLowerCase()
    val referenceNameLower = referenceName.toLowerCase()
    referenceNameToMatch.equals(referenceNameLower) || referenceNameToMatch.takeWhile(!_.isDigit).replace(" ", "").equals(referenceNameLower)
  }

  protected def enrichAbstractionWithSpecialization(reference: DocReference, specializations: Set[DocReference]): DocReference = {
    require(reference.enrichedLine.isDefined)
    val newEnriched = reference.enrichedLine.get + latexFormatter.addSpecialization(specializations, reference.documentName)
    reference.copy(
      enrichedLine = Some(newEnriched),
      abstracts = Some(specializations)
    )
  } ensuring ((ref: DocReference) => ref.enrichedLine.get.contains(reference.enrichedLine.get) && ref.abstracts.isDefined)

  protected def enrichSpecializationWithAbstraction(reference: DocReference, abstractions: Set[DocReference]): DocReference = {
    require(reference.enrichedLine.isDefined)
    val newEnriched = reference.enrichedLine.get + latexFormatter.addAbstractions(abstractions, reference.documentName)
    reference.copy(
      enrichedLine = Some(newEnriched),
      specializes = Some(abstractions)
    )
  } ensuring ((ref: DocReference) => ref.enrichedLine.get.contains(reference.enrichedLine.get) && ref.specializes.isDefined)
}

