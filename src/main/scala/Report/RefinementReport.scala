package Report

import Analyzers.ReportAnalyzer
import Formatter.LatexSanitizer
import Report.ReportTypes.ReportReference
import Types.DocReference.DocReference
import Types.LatexReferenceTypes

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.collection.mutable

object RefinementReport {
  private val refinementSymbol: String = "|-"
  def buildReport(report: ReportReference): String = {
    val latexDocument = generateRefinementReport(report)
    val latexFile = new File(report.folder, "refinement_report.tex")
    Files.write(Paths.get(latexFile.getAbsolutePath), latexDocument.toLatex.getBytes(StandardCharsets.UTF_8))
    LatexGenerator.buildLatexFile(latexFile, buildTwice = true)
    latexFile.getAbsolutePath
  }

  private def generateRefinementReport(report: ReportReference): LatexDocument = {
    val noneRefinedReferences = report.getNonRefinedReferences
    val refinedReferences = report.getRefinedReferences
    val title = "Refinement Report"
    val author = "Refinement Report"

    val documentNameToFilePath = report.allDocumentNamesToPaths
    //Sort references by document name and type
    val topOfRefinementChain = ReportAnalyzer.topOfRefinementChain(refinedReferences)

    val refinementChains = topOfRefinementChain.map(ref => formatReferenceChain(ref))

    val refinementChainVisualisation = Environment("alltt", refinementChains.toList)
    val refinementSection = LatexSection("Refinement Chains", "refinement_chain", List(refinementChainVisualisation))

    val mapOfReferencesPerDocumentType = noneRefinedReferences.groupBy(_.getDocumentType)
      .map(docTypeRef => (docTypeRef._1, docTypeRef._2.groupBy(_.getDocumentName)))

    val subsections = mapOfReferencesPerDocumentType.map(docTypeRef => {
      val subsubSections = docTypeRef._2.map(docRef => {
        val href = ClickableLink(documentNameToFilePath(docRef._1), docRef._1, LatexReferenceTypes.File).toLatex
        val label = docRef._1
        val listsOfReferences = docRef._2.map(formatReference).toList.sorted
        val listBlock = ListBlock(listsOfReferences, "itemize")
        Subsubsection(href, label, List(listBlock))
      })
      Subsection(docTypeRef._1.toString, docTypeRef._1.toString, subsubSections.toList)
    })

    val nonRefinementSection = LatexSection("Overview over non-refined concepts", "non_refined", subsections.toList)

    LatexDocument(title, author, List(refinementSection, nonRefinementSection), report.layout)
  }


  private def formatReference(reference: DocReference): String = {
    reference.sanitizedName + " (" + LatexSanitizer.sanitizeName(reference.documentName) + ")"
  }

  private def formatReferenceChain(reference: DocReference): String = {
    @tailrec
    def formatReferenceChainRec(docReference: DocReference, acc: String): String = {
      docReference.getRefinements match {
        case Some(refinements) =>
          val arbitraryRefinement = refinements.head
          val refinementText = refinements.map(ref => {
            val text = s"${ref.documentName}.${ref.sanitizedName}"
            Text(text, identity, "red").toLatex
          }).mkString("\\{", ", ", "\\}")
          val newAcc = acc + refinementSymbol + refinementText
          formatReferenceChainRec(arbitraryRefinement, newAcc)
        case None => acc
      }
    }

    val startString = s"${reference.documentName}.${reference.sanitizedName}"
    val coloredStartString = Text(startString, identity, "red").toLatex
    formatReferenceChainRec(reference, coloredStartString)
  }
}
