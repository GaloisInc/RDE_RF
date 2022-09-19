package ConfigParser

import Report.ReportTypes.ReportReference
import Types.DocReference.DocReference
import Types.DocumentType

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

object ObjectConfigGenerator {

  private def generateNoneRefinements(nonRefineReferences: Set[DocReference]): String = {
    nonRefineReferences.map(ref => ref.documentName -> ref.getName).toSeq.sortBy(_._1).map {
      case (docName, refName) => s"""$docName.$refName -> File.Ref"""
    }.mkString(",\n")
  }

  def generateNonRefinedFile(report: ReportReference, name: String): String = {
    val nonRefineReferences = report.getNonRefinedReferences.filter(ref => ref.getDocumentType == DocumentType.Lando)
    val builder = new StringBuilder()
    builder.addAll(f"name = ${name}\n")
    builder.addAll(s"refinements = [${generateNoneRefinements(nonRefineReferences)}]\n")
    builder.toString()
  }

  def generateNoneRefinedFile(report: ReportReference): String = {
    val content = generateNonRefinedFile(report, report.title)
    val file = Files.write(Paths.get(report.folder, s"${report.title}.conf"), content.getBytes(StandardCharsets.UTF_8))
    file.toString
  }

  def generateRefinementStrings(refinedReferences: Set[DocReference]): String = {
    refinedReferences.map(ref => ref.documentName -> ref).toSeq.sortBy(_._1).flatMap {
      case (srcDocName, srcRef) => {
        val refinements = srcRef.getRefinements.get
        refinements.map(ref => ref.documentName -> ref.getName).toSeq.sortBy(_._1).map {
          case (docName, refName) => s"""${srcDocName}.${srcRef.getName} -> ${docName}.${refName}"""
        }
      }
    }.mkString(",\n")
  }

  def generateRefinedFile(report: ReportReference, name: String): String = {
    val refineReferences = report.getRefinedReferences.filter(_.getRefinements.nonEmpty)
    val builder = new StringBuilder()
    builder.addAll(f"name = $name\n")
    builder.addAll(s"refinements = [${generateRefinementStrings(refineReferences)}]\n")

    builder.toString()
  }

  def generateRefinedFile(report: ReportReference): String = {
    val content = generateRefinedFile(report, report.title)
    val file = Files.write(Paths.get(report.folder, s"${report.title}.conf"), content.getBytes(StandardCharsets.UTF_8))
    file.toString
  }

}
