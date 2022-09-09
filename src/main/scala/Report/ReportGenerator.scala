package Report

import Analyzers.{DocumentAnalyzer, ReportAnalyzer}
import Formatter.LatexSanitizer
import Types.DocReference

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.collection.mutable

object ReportGenerator {
  private val refinementSymbol: String = "=>"

  def generateRefinementReport(filesToAnalyse: Array[String], reportName: String, targetFolder: String): String = {
    val report = DocumentAnalyzer.generateReport(filesToAnalyse, reportName, targetFolder, false)
    val noneRefinedReferences = ReportAnalyzer.notRefinedConstructs(report)
    val refinedReferences = ReportAnalyzer.refinedConstructs(report)

    val topOfRefinementChain = ReportAnalyzer.topOfRefinementChain(refinedReferences)

    val refinementChains = topOfRefinementChain.map(ref => formatReferenceChain(ref, ref.sanitizedName))

    val reportString = mutable.StringBuilder()

    reportString.append(LatexGenerator.generateSection("Refinement"))

    reportString.append(LatexGenerator.emptyLine)

    refinementChains.foreach(refinementChain => {
      reportString.append(refinementChain)
      reportString.append(LatexGenerator.emptyLine)
    })

    reportString.append(LatexGenerator.generateSection("NonRefined"))
    reportString.append(LatexGenerator.emptyLine)

    val mapOfReferencesPerDocumentType = noneRefinedReferences.groupBy(_.getDocumentType)
      .map(docTypeRef => (docTypeRef._1, docTypeRef._2.groupBy(_.getDocumentName)))

    mapOfReferencesPerDocumentType.foreach(docTypeRef => {
      reportString.append(LatexGenerator.generateSubSection(docTypeRef._1.toString))
      docTypeRef._2.foreach(docRef => {
        reportString.append(LatexGenerator.generateSubSubSection(docRef._1))
        val listsOfReferences = docRef._2.map(formatReference).toList.sorted
        reportString.append(LatexGenerator.generateList(listsOfReferences))
      })
    })

    val latexDocument = LatexGenerator.generateLatexDocument(reportString.toString())

    val filePath = Files.write(Paths.get(report.folder, s"${report.title}.tex"), latexDocument.getBytes(StandardCharsets.UTF_8))

    LatexGenerator.buildLatexFile(new File(filePath.toString), buildTwice = true)

    filePath.toString
  }


  def formatReference(reference: DocReference): String = {
    reference.sanitizedName + " (" + LatexSanitizer.sanitizeName(reference.documentName) + ")"
  }

  @tailrec
  def formatReferenceChain(docReference: DocReference, acc: String): String = {
    docReference.getRefinements match
      case Some(refinements) =>
        val arbitraryRefinement = refinements.head
        //assert(refinements.forall(_.getRefinements == arbitraryRefinement.getRefinements), "All similar refinements must refine the same abstractions.")
        val refinementText = refinements.map(ref => ref.sanitizedName).mkString("{", ", ", "}")
        val newAcc = acc + refinementSymbol + refinementText
        formatReferenceChain(arbitraryRefinement, newAcc)
      case None => acc
  }

}
