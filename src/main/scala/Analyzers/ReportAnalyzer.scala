package Analyzers

import Report.ReportTypes.ReportReference
import Types.DocReference

object ReportAnalyzer {

  def notRefinedConstructs(report: ReportReference): Set[DocReference] = {
    require(report.landoDocuments.nonEmpty, "Report must contain at least one lando document")
    val allReferences = report.landoDocuments.flatMap(_.getAllReferences)
      ++ report.sysmlDocuments.flatMap(_.getAllReferences)
      ++ report.cryptolDocuments.flatMap(_.getAllReferences)

    val notReferenced = allReferences.filterNot(_.isInRefinementChain)

    notReferenced.toSet
  }

  def topOfRefinementChain(references: Set[DocReference]): Set[DocReference] = {
    require(references.nonEmpty, "Must have at least one reference")
    require(references.forall(_.isInRefinementChain), "All references must be in refinement chain")
    val topOfChain = references.filter(ref => ref.getAbstractions.isEmpty)
    topOfChain
  } ensuring((result: Set[DocReference]) => result.subsetOf(references), "Result must be a subset of the input")

  def refinedConstructs(report: ReportReference): Set[DocReference] = {
    require(report.landoDocuments.nonEmpty, "Report must contain at least one lando document")
    val allReferences = report.landoDocuments.flatMap(_.getAllReferences)
      ++ report.sysmlDocuments.flatMap(_.getAllReferences)
      ++ report.cryptolDocuments.flatMap(_.getAllReferences)

    val referencedConstructs = allReferences.filter(_.isInRefinementChain)

    referencedConstructs.toSet
  }

}
