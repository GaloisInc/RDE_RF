package Referencer

import Formatter.InlineFormatter
import Types.*
import Types.DocumentInfos.DocumentInfo

import java.util.Locale

abstract class Referencer(hammingDistanceMeasure: Double = 0.15) {
  def addRefinementRelations(documentToExtend: DocumentInfo, abstractDocuments: Array[DocumentInfo], refinedDocuments: Array[DocumentInfo]): DocumentInfo

  def addSpecializationsToDocument(abstractDocument: DocumentInfo, refinedDocuments: Array[DocumentInfo]): DocumentInfo

  def addAbstractionsToDocument(specializedDocument: DocumentInfo, documentsBeingRefined: Array[DocumentInfo]): DocumentInfo

  protected def findRefinementRelation(refiningReference: DocReference, referenceBeingRefined: Set[DocReference]): DocReference = {
    val referenceName = refiningReference.getName
    val abstractions = referenceBeingRefined.filter(isSpecialization(referenceName, _))
    if (abstractions.nonEmpty)
      referenceRefines(refiningReference, abstractions)
    else
      refiningReference
  } ensuring ((ref: DocReference) =>
    ref.getReferenceType == refiningReference.getReferenceType
      && ref.getDocumentName == refiningReference.getDocumentName
      && ref.getDocumentType == refiningReference.getDocumentType
      && ref.getName == refiningReference.getName
      && ref.getReferenceType == refiningReference.getReferenceType
      && ref.getRefinements == refiningReference.getRefinements)

  protected def addRefinements(reference: DocReference, specializedReferences: Set[DocReference]): DocReference = {
    require(reference.getRefinements.isEmpty, "Reference should not have any specializations yet")
    val refinements = specializedReferences.filter(ref => ref.getAbstractions.isDefined
      && ref.getAbstractions.get.contains(reference))
    if (refinements.nonEmpty)
      addRefinementsOfToDocReference(reference, refinements)
    else
      reference
  } ensuring ((ref: DocReference) =>
    ref.getReferenceType == reference.getReferenceType
      && ref.getDocumentName == reference.getDocumentName
      && ref.getDocumentType == reference.getDocumentType
      && ref.getName == reference.getName
      && ref.getReferenceType == reference.getReferenceType
      && ref.getAbstractions == reference.getAbstractions)

  def isSpecialization(referenceName: String, ref: DocReference): Boolean = {
    val referenceNameToMatch = ref.getName.toLowerCase(Locale.US)
    val referenceNameAcronym = ref.getAcronym.getOrElse("XXXXXXXXXXX").toLowerCase()
    val referenceNameLower = referenceName.toLowerCase()
    referenceNameToMatch.equalsIgnoreCase(referenceNameLower)
      || referenceNameToMatch.takeWhile(!_.isDigit).replace(" ", "").equalsIgnoreCase(referenceNameLower)
      || referenceNameAcronym.equalsIgnoreCase(referenceNameLower)
      || Hamming.computeRelHamming(referenceName, referenceNameToMatch) <= hammingDistanceMeasure
  }

  protected def addRefinementsOfToDocReference(reference: DocReference, refinements: Set[DocReference]): DocReference = {
    val newRefinements = reference.getRefinements.getOrElse(Set.empty) ++ refinements
    DocReference(reference.getDocumentName,
      reference.getReference,
      reference.getReferenceType,
      reference.getDocumentType,
      reference.getOriginalLine,
      reference.getAbstractions,
      Some(newRefinements))
  } ensuring ((ref: DocReference) => ref.getRefinements.isDefined)

  protected def referenceRefines(reference: DocReference, abstractions: Set[DocReference]): DocReference = {
    val newAbstractions = reference.getAbstractions.getOrElse(Set.empty) ++ abstractions
    DocReference(reference.getDocumentName,
      reference.getReference,
      reference.getReferenceType,
      reference.getDocumentType,
      reference.getOriginalLine,
      Some(newAbstractions),
      reference.getRefinements)
  } ensuring ((ref: DocReference) => ref.getAbstractions.isDefined)

}
