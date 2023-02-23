package Analyzers

import ConfigParser.{FileDocRef, RefinementModel}
import Referencer._
import Report.ReportTypes.Documents
import Types.DocReference.DocReference
import Types.DocumentInfos._
import Utils.Matcher

/**
 * This trait is used to add references to the documents
 */
trait DocumentReferencer {
  /**
   * This method adds references to the documents
   */
  def addReferences(documents: Documents, explicitReferences: Set[RefinementModel]): Documents

  /**
   * This method adds references between documents of the same type
   */
  private def addInternalReferences[T <: DocumentInfo[T]](document: T, references: Set[DocReference]): Unit = {
    val referencesToUpdate: Set[DocReference] = document.getAllReferences.filter(ref => ref.isReferencingAnything)
    referencesToUpdate.foreach(reference => {
      val potentialReferences: Map[String, DocReference] = {
        references.flatMap(r => {
          reference.getStringReferences.get
            .map(ref => Matcher.getReferenceName(ref.name, r.getReferenceName))
            .filter(_.isDefined)
            .map(_.get)
            .map(ref => (ref, r))
        }).toMap
      }
      potentialReferences.foreach(ref => reference.addReference(ref))
    })
  }

  /**
   * This method is a helper method that adds references between documents of different types
   */
  protected def addImplicitRefinements[T <: DocumentInfo[T]](docs: Array[T]): Array[T] = {
    docs.foreach(doc => addInternalReferences(doc, docs.flatMap(_.getAllReferences).toSet))
    docs
  } //ensuring((res: Array[T]) => FileSpecs.allFilesAnalyzed(docs, res), "Not all files were analyzed")


  protected def addExplicitRefinements(report: Documents, refinements: Set[RefinementModel]): Documents = {
    val allReferences: Map[String, Array[DocReference]] = report.getAllReferences.groupBy(ref => ref.documentName)

    def getReference(ref: RefinementModel, ext: RefinementModel => FileDocRef): Array[DocReference] = {
      allReferences(ext(ref).file).filter(_.getName.equalsIgnoreCase(ext(ref).ref))
    }

    //Both ends of the refinement must be in the report source code and must be valid references otherwise the refinement is ignored
    val allValidRefinements = refinements.filter(
      refinement =>
        allReferences.keySet.contains(refinement.srcRef.file) &&
          allReferences.keySet.contains(refinement.trgRef.file) &&
          getReference(refinement, _.srcRef).nonEmpty &&
          getReference(refinement, _.trgRef).nonEmpty)

    allValidRefinements.foldLeft(report)((accReport, refinement) => {
      val srcRef = getReference(refinement, _.srcRef).head
      val trgRef = getReference(refinement, _.trgRef).head
      val updatedSrcRef = srcRef.addRefinement(trgRef)
      val updatedTargetRef = trgRef.addAbstraction(srcRef)
      accReport.updateDocumentByName(updatedSrcRef.documentName, updatedSrcRef)
        .updateDocumentByName(updatedTargetRef.documentName, updatedTargetRef)
    })
  }
}

object DocumentReferencer extends DocumentReferencer {
  def addReferences(documents: Documents, explicitReferences: Set[RefinementModel]): Documents = {
    val enrichedBSVDocuments = documents.bsvDocuments.map(doc => BlueSpecReferencer.addRefinementRelations(doc, documents.cryptolDocuments, Array.empty[BSVDocumentInfo]))
    val enrichedSVDocuments = documents.svDocuments.map(doc => SystemVerilogReferencer.addRefinementRelations(doc, documents.cryptolDocuments, Array.empty[SVDocumentInfo]))

    val enrichedCryptolDocuments = documents.cryptolDocuments.map(doc => {
      val enrichedWithBSV = CryptolBSVReferencer.addRefinementRelations(doc, documents.sysmlDocuments, enrichedBSVDocuments)
      val enrichedWithSV = CryptolSVReferencer.addRefinementRelations(enrichedWithBSV, documents.fretDocuments, enrichedSVDocuments)
      enrichedWithSV
    })

    val enrichedSysMLDocuments = documents.sysmlDocuments.map(doc => SysMLReferencer.addRefinementRelations(doc, documents.landoDocuments, enrichedCryptolDocuments))
    val enrichedFretDocuments = documents.fretDocuments.map(doc => {
      val enrichedWithSysml = FretSysMLReferencer.addRefinementRelations(doc, documents.landoDocuments, enrichedSysMLDocuments)
      val enrichedWithCryptol = FretCryptolReferencer.addRefinementRelations(enrichedWithSysml, documents.landoDocuments, enrichedCryptolDocuments)
      enrichedWithCryptol
    })

    val enrichedLandoDocuments = documents.landoDocuments.map(doc => {
      val enrichedWithFret = LandoFretReferencer.addRefinementRelations(doc, Array.empty[LandoDocumentInfo], enrichedFretDocuments)
      val enrichedWithSysML = LandoSysMLReferencer.addRefinementRelations(enrichedWithFret, Array.empty[LandoDocumentInfo], enrichedSysMLDocuments)
      enrichedWithSysML
    })

    val landoDocuments: Array[LandoDocumentInfo] = addImplicitRefinements(enrichedLandoDocuments)
    val lobotDocuments: Array[LobotDocumentInfo] = addImplicitRefinements(documents.lobotDocuments)
    val sysmlDocuments: Array[SysMLDocumentInfo] = addImplicitRefinements(enrichedSysMLDocuments)
    val cryptolDocuments: Array[CryptolDocumentInfo] = addImplicitRefinements(enrichedCryptolDocuments)
    val sawDocuments: Array[SawDocumentInfo] = addImplicitRefinements(documents.sawDocuments)
    val svDocuments: Array[SVDocumentInfo] = addImplicitRefinements(enrichedSVDocuments)
    val bsvDocuments: Array[BSVDocumentInfo] = addImplicitRefinements(enrichedBSVDocuments)
    val cDocuments: Array[CDocumentInfo] = addImplicitRefinements(documents.cDocuments)
    val fretDocuments: Array[FretDocument] = addImplicitRefinements(enrichedFretDocuments)
    val extendedDocuments = Documents(landoDocuments, lobotDocuments, sysmlDocuments, cryptolDocuments, sawDocuments, bsvDocuments, svDocuments, cDocuments, fretDocuments)
    addExplicitRefinements(extendedDocuments, explicitReferences)
  }


}