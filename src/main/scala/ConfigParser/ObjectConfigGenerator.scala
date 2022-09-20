package ConfigParser

import Report.ReportTypes.ReportReference
import Types.DocReference.DocReference
import Types.DocumentType

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

object ObjectConfigGenerator {
  private def generateNoneRefinements(nonRefineReferences: Set[DocReference]): String = {
    nonRefineReferences.map(ref => ref.documentName -> ref.getName).toSeq.sortBy(_._1).map {
      case (docName, refName) => s"""\t\t\t$docName.$refName -> File.Ref"""
    }.mkString(",\n")
  }

  private def generateRefinementStrings(refinedReferences: Set[DocReference]): String = {
    refinedReferences.map(ref => ref.documentName -> ref).toSeq.sortBy(_._1).flatMap {
      case (srcDocName, srcRef) => {
        val refinements = srcRef.getRefinements.get
        refinements.map(ref => ref.documentName -> ref.getName).toSeq.sortBy(_._1).map {
          case (docName, refName) => s"""\t\t\t${srcDocName}.${srcRef.getName} -> ${docName}.${refName}"""
        }
      }
    }.mkString(",\n")
  }

  def generateRefinementConfigFile(report: ReportReference, reportName: String): String = {
    val refineReferences = report.getRefinedReferences.filter(_.getRefinements.nonEmpty)
    val nonRefineReferences = report.getNonRefinedReferences.filter(ref => Set(DocumentType.Lando, DocumentType.Cryptol, DocumentType.SysML).contains(ref.getDocumentType))
    val builder = new StringBuilder()
    builder.addAll(f"name = $reportName\n")
    builder.addAll(s"implicit_refinements = [${generateRefinementStrings(refineReferences)}]\n")
    builder.addAll(s"explicit_refinements = [${generateNoneRefinements(nonRefineReferences)}]\n")
    val file = Files.write(Paths.get(report.folder, s"refinements_${reportName}.conf"), builder.toString().getBytes(StandardCharsets.UTF_8))
    file.toString
  }
}
