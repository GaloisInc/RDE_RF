package ConfigParser

import Report.ReportTypes.ReportReference
import Types.DocReference.DocReference
import Types.DocumentType
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

object ConfigGenerator {
  private def generateMapWithRefinements(references: Set[DocReference]): Map[String, List[String]] = {
    val documentMap = references.groupBy(_.documentName)
    documentMap.map(document => {
      val documentName = document._1
      val documentReferences = document._2
      val documentReferencesString = documentReferences.flatMap(srcRef => {
        if (srcRef.getRefinements.isEmpty)
          List(s"$documentName.${srcRef.getName} -> File.Ref")
        else
          srcRef.getRefinements.get.map(refinement => {
            s"$documentName.${srcRef.getName} -> ${refinement.documentName}.${refinement.getName}"
          })
      }).toList
      documentName -> documentReferencesString
    })
  }

  def generateRefinementConfigFile(report: ReportReference, reportName: String): String = {
    val refinedReferences = report.getRefinedReferences.filter(_.getRefinements.nonEmpty)
    val nonRefineReferences = report.getNonRefinedReferences.filter(ref => Set(DocumentType.Lando, DocumentType.Cryptol, DocumentType.SysML).contains(ref.getDocumentType))
    val refinementFileConfig = RefinementFileConfig(reportName, generateMapWithRefinements(refinedReferences), generateMapWithRefinements(nonRefineReferences))
    val file = Files.write(Paths.get(report.folder, s"refinements_$reportName.conf"), refinementFileConfig.serialize(0).getBytes(StandardCharsets.UTF_8))
    file.toString
  } ensuring ((fileString: String) => {
    val file = Paths.get(fileString)
    assert(Files.exists(file), s"Refinement config file: $fileString does not exist")
    val fileContent = Files.readAllLines(file).toArray().mkString("\n")
    fileContent.contains("implicit-refinements") && fileContent.contains("explicit-refinements")
  })
}
