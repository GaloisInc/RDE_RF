package Utils

import Analyzers.{DocumentAnalyzer, LatexDocumentData}
import Formatter.LatexFormatter
import Report.PaperLayout
import Report.ReportTypes.ReportReference
import java.io.File

/**
 * Trait that contains methods to compile latex files and assert that the files exist
 */
trait LatexCompilationTester {
  def author = "TestAuthor"

  def reportPath: String = getClass.getResource(".").getPath

  /**
   * Compiles a latex file, asserts that the file exists and cleans up the file
   */
  private def compileLatexFile(filesTypes: Set[Types.DocumentType.documentType],
                               reportName: String,
                               paperLayout: PaperLayout.PaperLayout,
                               formatter: LatexFormatter,
                               reportGeneration: ReportReference => String,
                               sortFiles: Boolean,
                               refinementModels: Set[ConfigParser.RefinementModel],
                               folder: String = reportPath): Boolean = {
    val filesToAnalyze = ResourceFiles.getFilesOfTypes(filesTypes)
    val latexDocumentData = LatexDocumentData(reportName, author, folder, paperLayout, formatter)
    val report = DocumentAnalyzer.generateReport(filesToAnalyze, latexDocumentData, refinementModels, sortFiles)
    val reportFilePath = reportGeneration(report)
    //Ensure that the report is generated
    val reportFile = new File(reportFilePath)
    assert(reportFile.exists(), "Report file does not exist")
    val pdfPath = reportFile.getPath.replace(".tex", ".pdf")
    val pdfFile = new File(pdfPath)
    assert(pdfFile.exists(), "PDF file does not exist")
    //Delete the report file
    reportFile.delete()
    pdfFile.delete()
    true
  }

  def buildRefinementReport(filesTypes: Set[Types.DocumentType.documentType],
                            reportName: String,
                            paperLayout: PaperLayout.PaperLayout,
                            formatter: LatexFormatter,
                            refinementModels: Set[ConfigParser.RefinementModel],
                            sortFiles: Boolean,
                            folder: String = reportPath
                           ): Boolean = {
    compileLatexFile(Set(Types.DocumentType.SysML, Types.DocumentType.Lando, Types.DocumentType.Cryptol, Types.DocumentType.Fret),
      reportName,
      paperLayout,
      formatter,
      _.buildRefinementReport,
      sortFiles,
      refinementModels,
      folder)
  }

  def buildDocumentationReport(filesTypes: Set[Types.DocumentType.documentType],
                               reportName: String,
                               paperLayout: PaperLayout.PaperLayout,
                               formatter: LatexFormatter,
                               refinementModels: Set[ConfigParser.RefinementModel],
                               sortFiles: Boolean,
                               folder: String = reportPath): Boolean = {
    compileLatexFile(filesTypes, reportName, paperLayout, formatter, _.buildDocumentationReport, sortFiles, refinementModels, folder)
  }
}