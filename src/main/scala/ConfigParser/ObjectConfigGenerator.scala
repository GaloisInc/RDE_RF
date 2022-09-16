package ConfigParser

import Report.ReportTypes.ReportReference
import Types.DocReference.DocReference

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

object ObjectConfigGenerator {

  def generateRefinements(nonRefineReferences: Set[DocReference]): String = {
    nonRefineReferences.map(ref => ref.documentName -> ref.getName).toSeq.sortBy(_._1).map {
      case (docName, refName) => s"""$docName.$refName -> File.Ref"""
    }.mkString(",\n")
  }

  def generate(report: ReportReference, name: String): String = {
    val nonRefineReferences = report.getNonRefinedReferences
    val builder = new StringBuilder()
    builder.addAll(f"name = ${name}\n")
    builder.addAll(s"refinements = [${generateRefinements(nonRefineReferences)}]\n")

    builder.toString()
  }

  def generate(report: ReportReference): String = {
    val content = generate(report, report.title)
    val file = Files.write(Paths.get(report.folder, s"${report.title}.conf"), content.getBytes(StandardCharsets.UTF_8))
    file.toString
  }

}
