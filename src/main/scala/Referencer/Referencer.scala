package Referencer

import Types.DocReference.DocReference
import Types.DocumentInfos.{DocumentInfo, DocumentInfoCompare}
import org.apache.logging.log4j.scala.Logging

abstract class Referencer[T <: DocumentInfo[T], A <: DocumentInfo[A], R <: DocumentInfo[R]](hammingDistanceMeasure: Double = 0.15) extends Logging {
  def addRefinementRelations(documentToExtend: T, abstractDocuments: Array[A], refinedDocuments: Array[R]): T = {
    logger.info("Adding refinement relations to " + documentToExtend.documentName + " from " + abstractDocuments.map(_.documentName).mkString(", "))
    addAbstractionsToDocument(documentToExtend, abstractDocuments)
    logger.info("Adding specializations to " + documentToExtend.documentName + " from " + refinedDocuments.map(_.documentName).mkString(", "))
    addSpecializationsToDocument(documentToExtend, refinedDocuments)
  } ensuring ((resDoc: T) => DocumentInfoCompare.compareAddedReferences(resDoc, documentToExtend))

  def addSpecializationsToDocument(documentInfo: T, refinedDocuments: Array[R]): T = {
    logger.info("Adding specializations to " + documentInfo.documentName + " from " + refinedDocuments.map(_.documentName).mkString(", "))
    val refinedReferences = refinedDocuments.flatMap(doc => doc.getAllReferences.filter(_.getAbstractions.nonEmpty))
    val updatedReferences = documentInfo.getAllReferences.map(reference => addRefinements(reference, refinedReferences.toSet))
    documentInfo.updateReferences(updatedReferences)
  } ensuring ((resDoc: T) => DocumentInfoCompare.compareAddedReferences(resDoc, documentInfo))

  def addAbstractionsToDocument(refinedDocument: T, documentsBeingRefined: Array[A]): T = {
    logger.info("Adding abstractions to " + refinedDocument.documentName + " from " + documentsBeingRefined.map(_.documentName).mkString(", "))
    val cryptolReferences = documentsBeingRefined.flatMap(doc => doc.getAllReferences)
    val updatedReferences = refinedDocument.getAllReferences.map(reference => findRefinementRelation(reference, cryptolReferences.toSet))
    refinedDocument.updateReferences(updatedReferences)
  } ensuring ((resDoc: T) => DocumentInfoCompare.compareAddedReferences(resDoc, refinedDocument))

   private def findRefinementRelation(refiningReference: DocReference, referenceBeingRefined: Set[DocReference]): DocReference = {
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

  private def addRefinements(reference: DocReference, specializedReferences: Set[DocReference]): DocReference = {
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
    val referenceNameAcronym: String = ref.getAcronym.getOrElse("XXXXXXXXXXX")
    referenceNameToMatch.equalsIgnoreCase(nameOfAbstractReference) ||
      referenceNameToMatch.takeWhile(!_.isDigit).replace(" ", "").equalsIgnoreCase(nameOfAbstractReference) ||
      referenceNameAcronym.equalsIgnoreCase(acronymOfAbstractReference) ||
      referenceNameToMatch.equalsIgnoreCase(acronymOfAbstractReference) ||
      Hamming.computeRelHamming(nameOfAbstractReference, referenceNameToMatch) <= hammingDistanceMeasure
  }

  private def addRefinementsOfToDocReference(reference: DocReference, refinements: Set[DocReference]): DocReference = {
    val newRefinements = reference.getRefinements.getOrElse(Set.empty) ++ refinements
    reference.copy(abstractionOf = Some(newRefinements))
  } ensuring ((ref: DocReference) => ref.getRefinements.isDefined)

  private def referenceRefines(reference: DocReference, abstractions: Set[DocReference]): DocReference = {
    val newAbstractions = reference.getAbstractions.getOrElse(Set.empty) ++ abstractions
    reference.copy(refinementOf = Some(newAbstractions))
  } ensuring ((ref: DocReference) => ref.getAbstractions.isDefined)
}
