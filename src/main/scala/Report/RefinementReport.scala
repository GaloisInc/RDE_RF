package Report

import Analyzers.{DocumentAnalyzer, ReportAnalyzer}
import Formatter.{LatexSanitizer, LatexSyntax}
import Report.ReportTypes.ReportReference
import Types.DocReference.DocReference
import Types.LatexReferenceType

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.collection.mutable

object RefinementReport {
  private val refinementSymbol: String = "|-"

  def generateRefinementReport(report: ReportReference): String = {
    val noneRefinedReferences = ReportAnalyzer.notRefinedConstructs(report)
    val refinedReferences = ReportAnalyzer.refinedConstructs(report)

    val documentNameToFilePath = report.allDocumentNamesToPaths

    val topOfRefinementChain = ReportAnalyzer.topOfRefinementChain(refinedReferences)

    val refinementChains = topOfRefinementChain.map(ref => formatReferenceChain(ref, s"${ref.documentName}.${ref.sanitizedName}"))

    val reportString = mutable.StringBuilder()

    reportString.append(LatexSyntax.generateSection("Refinement"))

    reportString.append(LatexGenerator.emptyLine)

    reportString.append(LatexGenerator.addContentInsideEnvironment(refinementChains.toArray, "alltt"))

    reportString.append(LatexSyntax.generateSection("NonRefined"))
    reportString.append(LatexGenerator.emptyLine)

    val mapOfReferencesPerDocumentType = noneRefinedReferences.groupBy(_.getDocumentType)
      .map(docTypeRef => (docTypeRef._1, docTypeRef._2.groupBy(_.getDocumentName)))

    mapOfReferencesPerDocumentType.foreach(docTypeRef => {
      reportString.append(LatexSyntax.generateSubSection(docTypeRef._1.toString))
      docTypeRef._2.foreach(docRef => {
        val subsecHref = LatexSyntax.addClickableLocalLink(documentNameToFilePath(docRef._1), docRef._1, LatexReferenceType.File)
        val subsecLabel = LatexSanitizer.sanitizeReferenceName(docRef._1)
        reportString.append(LatexSyntax.generateSubSubSection(subsecHref, subsecLabel))
        val listsOfReferences = docRef._2.map(formatReference).toList.sorted
        reportString.append(LatexGenerator.generateList(listsOfReferences))
      })
    })

    val latexDocument = LatexGenerator.generateLatexDocument(reportString.toString(), report.layout)

    val filePath = Files.write(Paths.get(report.folder, s"${report.title}.tex"), latexDocument.getBytes(StandardCharsets.UTF_8))

    LatexGenerator.buildLatexFile(new File(filePath.toString), buildTwice = true)

    filePath.toString
  }


  private def formatReference(reference: DocReference): String = {
    reference.sanitizedName + " (" + LatexSanitizer.sanitizeName(reference.documentName) + ")"
  }

  @tailrec
  def formatReferenceChain(docReference: DocReference, acc: String): String = {
    docReference.getRefinements match
      case Some(refinements) =>
        val arbitraryRefinement = refinements.head
        //assert(refinements.forall(_.getRefinements == arbitraryRefinement.getRefinements), "All similar refinements must refine the same abstractions.")
        val refinementText = refinements.map(ref => s"${ref.documentName}.${ref.sanitizedName}").mkString("\\{", ", ", "\\}")
        val newAcc = acc + refinementSymbol + refinementText
        formatReferenceChain(arbitraryRefinement, newAcc)
      case None => acc
  }

}
