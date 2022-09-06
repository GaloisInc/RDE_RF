package Referencer

import Formatter.InlineFormatter
import Types.*
import Types.DocumentInfos.DocumentInfo

import java.util.Locale

abstract class Referencer(hammingDistanceMeasure: Double = 0.15) {
  def addSpecializationAndAbstract(documentToExtend: DocumentInfo, abstractDocuments: Array[DocumentInfo], specializedDocuments: Array[DocumentInfo]): DocumentInfo

  def addSpecializationsToDocument(abstractDocument: DocumentInfo, specializedDocuments: Array[DocumentInfo]): DocumentInfo

  def addAbstractionsToDocument(specializedDocument: DocumentInfo, abstractDocuments: Array[DocumentInfo]): DocumentInfo

  protected def addAbstractions(reference: DocReference, abstractReferences: Set[DocReference]): DocReference = {
    val referenceName = reference.getName
    val abstractions = abstractReferences.filter(isSpecialization(referenceName, _))
    if (abstractions.nonEmpty)
      enrichSpecializationWithAbstraction(reference, abstractions)
    else
      reference
  } ensuring ((ref: DocReference) =>
    ref.getReferenceType == reference.getReferenceType
      && ref.getDocumentName == reference.getDocumentName
      && ref.getDocumentType == reference.getDocumentType
      && ref.getName == reference.getLabelText
      && ref.getReferenceType == reference.getReferenceType
      && ref.getAbstracts == reference.getAbstracts)

  protected def addSpecializations(reference: DocReference, specializedReferences: Set[DocReference]): DocReference = {
    require(reference.getSpecializes.isEmpty, "Reference should not have any specializations yet")
    val specializations = specializedReferences.filter(ref => ref.getSpecializes.isDefined && ref.getSpecializes.get.contains(reference))
    if (specializations.nonEmpty)
      enrichAbstractionWithSpecialization(reference, specializations)
    else
      reference
  } ensuring ((ref: DocReference) =>
    ref.getReferenceType == reference.getReferenceType
      && ref.getDocumentName == reference.getDocumentName
      && ref.getDocumentType == reference.getDocumentType
      && ref.getName == reference.getName
      && ref.getReferenceType == reference.getReferenceType
      && ref.getSpecializes == reference.getSpecializes)

  def isSpecialization(referenceName: String, ref: DocReference): Boolean = {
    val referenceNameToMatch = ref.getName.toLowerCase(Locale.US)
    val referenceNameAcronym = ref.getAcronym.getOrElse("XXXXXXXXXXX").toLowerCase()
    val referenceNameLower = referenceName.toLowerCase()
    referenceNameToMatch.equals(referenceNameLower)
      || referenceNameToMatch.takeWhile(!_.isDigit).replace(" ", "").equals(referenceNameLower)
      || referenceNameAcronym.equals(referenceNameLower)
      || Hamming.computeRelHamming(referenceName, referenceNameToMatch) <= hammingDistanceMeasure
  }

  protected def enrichAbstractionWithSpecialization(reference: DocReference, specializations: Set[DocReference]): DocReference = {
    val newSpecializations = reference.getSpecializes.getOrElse(Set.empty) ++ specializations
    DocReference(reference.getDocumentName,
      reference.getReference,
      reference.getReferenceType,
      reference.getDocumentType,
      reference.getOriginalLine,
      reference.getAbstracts,
      Some(newSpecializations))
  } ensuring ((ref: DocReference) => ref.getAbstracts.isDefined)

  protected def enrichSpecializationWithAbstraction(reference: DocReference, abstractions: Set[DocReference]): DocReference = {
    val newAbstractions = reference.getAbstracts.getOrElse(Set.empty) ++ abstractions
    DocReference(reference.getDocumentName,
      reference.getReference,
      reference.getReferenceType,
      reference.getDocumentType,
      reference.getOriginalLine,
      Some(newAbstractions),
      reference.getSpecializes)
  } ensuring ((ref: DocReference) => ref.getSpecializes.isDefined)

}
