package Report

import Analyzers.ReportAnalyzer
import Formatter.{LatexSanitizer, LatexSyntax}
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

  def generateRefinementReport(report: ReportReference): String = {
    val noneRefinedReferences = report.getNonRefinedReferences
    val refinedReferences = report.getRefinedReferences

    val documentNameToFilePath = report.allDocumentNamesToPaths

    val topOfRefinementChain = ReportAnalyzer.topOfRefinementChain(refinedReferences)

    val refinementChains = topOfRefinementChain.map(ref => formatReferenceChain(ref))

    val reportString = new mutable.StringBuilder

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
        val subsecHref = LatexSyntax.addClickableLocalLink(documentNameToFilePath(docRef._1), docRef._1, LatexReferenceTypes.File)
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

  def formatReferenceChain(reference: DocReference): String = {
    @tailrec
    def formatReferenceChainRec(docReference: DocReference, acc: String): String = {
      docReference.getRefinements match {
        case Some(refinements) =>
          val arbitraryRefinement = refinements.head
          val refinementText = refinements.map(ref => {
            val text = s"${ref.documentName}.${ref.sanitizedName}"
            LatexSyntax.colorText(text, ref.getDocumentType)
          }).mkString("\\{", ", ", "\\}")
          val newAcc = acc + refinementSymbol + refinementText
          formatReferenceChainRec(arbitraryRefinement, newAcc)
        case None => acc
      }
    }

    val startString = s"${reference.documentName}.${reference.sanitizedName}"
    val coloredStartString = LatexSyntax.colorText(startString, reference.getDocumentType)
    formatReferenceChainRec(reference, coloredStartString)
  }
}
