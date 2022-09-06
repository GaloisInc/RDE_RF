package Report

import Analyzers.DocumentAnalyzer
import Types.DocReference

import scala.annotation.tailrec

object ReportGenerator {
  private val refinementSymbol: String = "|="

  def generateReport(filesToAnalyse: Array[String]): String = {
    val nonSpecializedLandoConstructs = DocumentAnalyzer.nonRefinementLando(filesToAnalyse)

    ""
  }


  @tailrec
  def formatReferenceChain(docReference: DocReference, acc: String): String = {
    docReference.getAbstracts match
      case Some(refinements) =>
        val arbitraryRefinement = refinements.head
        assert(refinements.forall(_.getSpecializes == arbitraryRefinement.getSpecializes), "All similar refinements must refine the same abstractions.")
        val refinementText = refinements.map(ref => ref.getName).mkString("{", ", ", "}")
        val newAcc = acc + refinementSymbol + refinementText
        formatReferenceChain(arbitraryRefinement, newAcc)
      case None => acc
  }

}
