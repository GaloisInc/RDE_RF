package Report

import Analyzers.{DocumentAnalyzer, ReportAnalyzer}
import Types.DocReference

import scala.annotation.tailrec

object ReportGenerator {
  private val refinementSymbol: String = "|="

  def generateReport(filesToAnalyse: Array[String]): String = {
    val report = DocumentAnalyzer.generateReport(filesToAnalyse, "", "", false)
    val noneRefinedReferences = ReportAnalyzer.notRefinedConstructs(report)

    ""

  }


  @tailrec
  def formatReferenceChain(docReference: DocReference, acc: String): String = {
    docReference.getAbstractions match
      case Some(refinements) =>
        val arbitraryRefinement = refinements.head
        assert(refinements.forall(_.getRefinements == arbitraryRefinement.getRefinements), "All similar refinements must refine the same abstractions.")
        val refinementText = refinements.map(ref => ref.getName).mkString("{", ", ", "}")
        val newAcc = acc + refinementSymbol + refinementText
        formatReferenceChain(arbitraryRefinement, newAcc)
      case None => acc
  }

}
