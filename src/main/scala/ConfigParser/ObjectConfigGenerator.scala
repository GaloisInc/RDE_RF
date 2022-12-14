package ConfigParser

import Report.ReportTypes.ReportReference
import Types.DocReference.DocReference
import Types.DocumentType

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

object ObjectConfigGenerator {
  private def generateNoneRefinements(nonRefineReferences: Set[DocReference]): String = {
    val documentMap = nonRefineReferences.groupBy(_.documentName)
    documentMap.map(document => {
      val documentName = document._1
      val documentReferences = document._2
      val documentReferencesString = documentReferences.map(reference => {
        s"""\t\t\t$documentName.${reference.getName} -> File.Ref"""
      }).mkString("\n", "\n", "\n")
      s"""\t$documentName = [$documentReferencesString],"""
    }).mkString("\n", "\n", "\n")
  }

  private def generateRefinementStrings(refinedReferences: Set[DocReference]): String = {
    val documentMap = refinedReferences.groupBy(_.documentName)
    documentMap.map(document => {
      val documentName = document._1
      val documentReferences = document._2
      val documentReferencesString = documentReferences.flatMap(srcRef => {
        srcRef.getRefinements.get.map(refinement => {
          s"""\t\t\t$documentName.${srcRef.getName} -> ${refinement.documentName}.${refinement.getName}"""
        })
      }).mkString("\n", "\n", "\n")
      s"""\t$documentName = [$documentReferencesString],"""
    }).mkString("\n", "\n", "\n")
  }

  def generateRefinementConfigFile(report: ReportReference, reportName: String): String = {
    val refinedReferences = report.getRefinedReferences.filter(_.getRefinements.nonEmpty)
    val nonRefineReferences = report.getNonRefinedReferences.filter(ref => Set(DocumentType.Lando, DocumentType.Cryptol, DocumentType.SysML).contains(ref.getDocumentType))
    val builder = new StringBuilder()
    builder.addAll(f"name = $reportName\n")
    builder.addAll(s"implicit-refinements = {${generateRefinementStrings(refinedReferences)}}\n")
    builder.addAll(s"explicit-refinements = {${generateNoneRefinements(nonRefineReferences)}}\n")
    val file = Files.write(Paths.get(report.folder, s"refinements_${reportName}.conf"), builder.toString().getBytes(StandardCharsets.UTF_8))
    file.toString
  } ensuring ((fileString: String) => {
    val file = Paths.get(fileString)
    assert(Files.exists(file), s"Refinement config file: $fileString does not exist")
    val fileContent = Files.readAllLines(file).toArray().mkString("\n")
    fileContent.contains("implicit-refinements") && fileContent.contains("explicit-refinements")
  })
}
