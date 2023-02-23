package Referencer

import Types.DocReference.DocReference
import Types.DocumentInfos.DocumentInfo
import Utils.DocumentInfoCompare
import org.apache.logging.log4j.scala.Logging

abstract class Referencer[T <: DocumentInfo[T], A <: DocumentInfo[A], R <: DocumentInfo[R]](hammingDistanceMeasure: Double = 0.15) extends Logging {
  def addRefinementRelations(documentToExtend: T, abstractDocuments: Array[A], refinedDocuments: Array[R]): T = {
    logger.info("Adding refinement relations to " + documentToExtend.documentName + " from " + abstractDocuments.map(_.documentName).mkString(", "))
    val documentWithAbstractions = addAbstractionsToDocument(documentToExtend, abstractDocuments)
    logger.info("Adding specializations to " + documentToExtend.documentName + " from " + refinedDocuments.map(_.documentName).mkString(", "))
    addSpecializationsToDocument(documentWithAbstractions, refinedDocuments)
  } ensuring ((resDoc: T) => DocumentInfoCompare.compareAddedReferences(resDoc, documentToExtend))

  private def addSpecializationsToDocument(document: T, refinedDocuments: Array[R]): T = {
    logger.info("Adding specializations to " + document.documentName + " from " + refinedDocuments.map(_.documentName).mkString(", "))
    val refinedReferences = refinedDocuments.flatMap(doc => doc.getAllReferences.filter(_.getAbstractions.nonEmpty))
    val updatedReferences = document.getAllReferences.map(reference => addRefinements(reference, refinedReferences.toSet))
    val doc = document.updateReferences(updatedReferences)
    doc
  } ensuring ((resDoc: T) => DocumentInfoCompare.compareAddedReferences(resDoc, document))

  private def addAbstractionsToDocument(document: T, documentsBeingRefined: Array[A]): T = {
    logger.info("Adding abstractions to " + document.documentName + " from " + documentsBeingRefined.map(_.documentName).mkString(", "))
    val abstractReferences = documentsBeingRefined.flatMap(doc => doc.getAllReferences)
    val updatedReferences = document.getAllReferences.map(reference => findRefinementRelation(reference, abstractReferences.toSet))
    val doc = document.updateReferences(updatedReferences)
    doc
  } ensuring ((resDoc: T) => DocumentInfoCompare.compareAddedReferences(resDoc, document))

  private def findRefinementRelation(reference: DocReference, abstractReferences: Set[DocReference]): DocReference = {
    // Find all references that are abstractions of the reference
    val abstractions = abstractReferences.filter(isSpecialization(reference, _))
    if (abstractions.nonEmpty)
      referenceRefines(reference, abstractions)
    else
      reference
  } ensuring ((ref: DocReference) =>
    ref.getReferenceType == reference.getReferenceType
      && ref.getDocumentName == reference.getDocumentName
      && ref.getDocumentType == reference.getDocumentType
      && ref.getName == reference.getName
      && ref.getReferenceType == reference.getReferenceType
      && ref.getRefinements == reference.getRefinements
      && ref.getAbstractions.size >= reference.getAbstractions.size)

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
      && ref.getAbstractions == reference.getAbstractions
      && ref.getRefinements.size >= reference.getRefinements.size)

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
