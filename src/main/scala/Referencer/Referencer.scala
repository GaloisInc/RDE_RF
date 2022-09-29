package Referencer

import Types.DocReference.DocReference
import Types.DocumentInfos.DocumentInfo
import org.apache.logging.log4j.scala.Logging

abstract class Referencer(hammingDistanceMeasure: Double = 0.15) extends Logging{
  def addRefinementRelations(documentToExtend: DocumentInfo, abstractDocuments: Array[DocumentInfo], refinedDocuments: Array[DocumentInfo]): DocumentInfo

  def addSpecializationsToDocument(abstractDocument: DocumentInfo, refinedDocuments: Array[DocumentInfo]): DocumentInfo

  def addAbstractionsToDocument(specializedDocument: DocumentInfo, documentsBeingRefined: Array[DocumentInfo]): DocumentInfo

  protected def findRefinementRelation(refiningReference: DocReference, referenceBeingRefined: Set[DocReference]): DocReference = {
    val abstractions = referenceBeingRefined.filter(isSpecialization(refiningReference, _))
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

  def isSpecialization(AbstractReference: DocReference, ref: DocReference): Boolean = {
    val nameOfAbstractReference = AbstractReference.getName
    val acronymOfAbstractReference = AbstractReference.getAcronym.getOrElse("YYYYYYYYYY")
    val referenceNameToMatch = ref.getName
    val referenceNameAcronym : String = ref.getAcronym.getOrElse("XXXXXXXXXXX")
    referenceNameToMatch.equalsIgnoreCase(nameOfAbstractReference) ||
      referenceNameToMatch.takeWhile(!_.isDigit).replace(" ", "").equalsIgnoreCase(nameOfAbstractReference) ||
        referenceNameAcronym.equalsIgnoreCase(acronymOfAbstractReference) ||
        referenceNameToMatch.equalsIgnoreCase(acronymOfAbstractReference) ||
        Hamming.computeRelHamming(nameOfAbstractReference, referenceNameToMatch) <= hammingDistanceMeasure
  }

  protected def addRefinementsOfToDocReference(reference: DocReference, refinements: Set[DocReference]): DocReference = {
    val newRefinements = reference.getRefinements.getOrElse(Set.empty) ++ refinements
    reference.copy(abstractionOf = Some(newRefinements))
  } ensuring ((ref: DocReference) => ref.getRefinements.isDefined)

  protected def referenceRefines(reference: DocReference, abstractions: Set[DocReference]): DocReference = {
    val newAbstractions = reference.getAbstractions.getOrElse(Set.empty) ++ abstractions
    reference.copy(refinementOf = Some(newAbstractions))
  } ensuring ((ref: DocReference) => ref.getAbstractions.isDefined)
}
