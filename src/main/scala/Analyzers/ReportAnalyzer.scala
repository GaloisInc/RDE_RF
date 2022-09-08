package Analyzers

import Report.ReportTypes.ReportReference
import Types.DocReference

class ReportAnalyzer {




  def notRefinedConstructs(report: ReportReference): Set[DocReference] = {
    require(report.landoDocuments.nonEmpty, "Report must contain at least one lando document")
    val allReferences = report.landoDocuments.flatMap(_.getAllReferences)
      ++ report.sysmlDocuments.flatMap(_.getAllReferences)
      ++ report.cryptolDocuments.flatMap(_.getAllReferences)

    val notReferenced = allReferences.filterNot(_.isReferenced)

    notReferenced.toSet
  }

}
