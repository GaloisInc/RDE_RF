package Analyzers

import Report.ReportTypes.ReportReference
import Types.DocReference.DocReference

object ReportAnalyzer {
  def topOfRefinementChain(references: Set[DocReference]): Set[DocReference] = {
    require(references.nonEmpty, "Must have at least one reference")
    require(references.forall(_.isInRefinementChain), "All references must be in refinement chain")
    val topOfChain = references.filter(ref => ref.getAbstractions.isEmpty)
    topOfChain
  } ensuring((result: Set[DocReference]) => result.subsetOf(references), "Result must be a subset of the input")
}
