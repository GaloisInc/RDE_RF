package Cli

import Analyzers.{DocumentAnalyzer, LatexDocumentData, SourceVerifier}
import ConfigParser.{ConfigGenerator, RefinementLoader, RefinementModel}
import EnvironmentChecker.EnvironmentChecker
import Formatter.{InlineFormatter, LatexFormatter, MarginFormatter}
import Report.PaperLayout
import Report.PaperLayout.PaperLayout
import Report.ReportTypes.ReportReference
import Specs.FileSpecs
import Utils.FileUtil
import org.apache.logging.log4j.scala.Logging
import scopt.OParser

import java.awt.Desktop
import java.io.File

object DocumentationEnhancerApp extends App with Logging {
  private val fileTypesOfTypesOfInterest = Analyzers.AnalyzerSettings.supportedDocumentTypesString

  val parser: OParser[Unit, CLIConfig] = CommandLineParser.createParser

  OParser.parse(parser, args, CLIConfig(), OParserSetup()) match {
    case Some(config) =>
      val sourceFolder = new File(config.sourceFolder).getAbsolutePath
      val targetFolder = new File(config.targetFolder).getAbsolutePath
      val latexTitle = if (config.latexTitle.isEmpty) "Documentation" else config.latexTitle
      val explicitRefinements = config.refinementFile
      val refinementFile = new File(explicitRefinements)
      // Ensure that the target folder does not contain any previous files
      FileUtil.deleteRecursivelyDecoratedFiles(targetFolder)

      val sourceFiles = FileUtil.findSourceFiles(sourceFolder, fileTypesOfTypesOfInterest)
      val excludedFolders = config.excludeFolders
      val sourceFilesWithoutExcluded = sourceFiles.filterNot(file => excludedFolders.exists(folder => file.contains(folder)))
      println("Starting Documentation Enhancer")

      if (sourceFilesWithoutExcluded.isEmpty) {
        println("No source files found in source folder: " + sourceFolder)
        System.exit(1)
      }

      FileUtil.allFilesReadable(sourceFilesWithoutExcluded)
      verifySourceFiles(config, sourceFilesWithoutExcluded)

      val latexDimensions = layoutStringToPaperSize(config.latexLayout)

      val latexGenerationData = LatexDocumentData(latexTitle, config.author, targetFolder, latexDimensions._1, latexDimensions._2)
      val documentReport: ReportReference = generateReport(refinementFile, sourceFilesWithoutExcluded, latexGenerationData)

      println("The files have been enriched and sorted into different folders in the folder " + targetFolder + ".")
      if (config.generateLatex) {
        println("You have chosen to generate a LaTeX document. " +
          "The document will be generated in the folder " + targetFolder + ".")
        val filePath = documentReport.buildDocumentationReport
        val pdfFile = new File(filePath.replace(".tex", ".pdf"))
        println("The LaTeX files have been generated and compiled in the folder " + targetFolder + ".")
        // Open the pdf file
        if (Desktop.isDesktopSupported) {
          Desktop.getDesktop.open(pdfFile)
        }
      }
      if (config.generateRefinementFile) {
        ConfigGenerator.generateRefinementConfigFile(documentReport, "refinementOverview")
        println("The refinement overview has been generated in the folder " + targetFolder + ".")
      }
      println("Done!")
      System.exit(0)
    case _ =>
      println("Invalid arguments! Please check the readme for more information and try again.")
      System.exit(1)
  }

  private def verifySourceFiles(config: CLIConfig, files: Array[String]): Unit = {
    require(files.nonEmpty, "No files found in source folder")
    if (config.verifySourceFiles) {
      println("Checking that all dependencies are installed in the environment")
      if (!EnvironmentChecker.dependenciesInstalled)
        logger.error("Not all dependencies are installed, please install them before using the tool.")
      else
        logger.info("All Dependencies are installed")
      logger.info("Verifying source files")
      val nonVerifiedSourceFiles = SourceVerifier.verifySourceFiles(files)
      if (nonVerifiedSourceFiles.nonEmpty) {
        logger.info("The following source files could not be verified: ")
        nonVerifiedSourceFiles.foreach(f => logger.info(f))
        System.exit(1)
      }
    }
  }

  private def generateReport(refinementFile: File, files: Array[String], latexGenerationData: LatexDocumentData): ReportReference = {
    require(FileSpecs.fileChecks(files.toSet, fileTypesOfTypesOfInterest))
    files.foreach(file => println("Processing file: " + file))

    val explicitRefinements = if (refinementFile.exists())
      RefinementLoader.load(refinementFile.getAbsolutePath).explicit_refinements.values.flatten.toSet
    else Set.empty[RefinementModel]

    val documentReport = DocumentAnalyzer.generateReport(files.toSet, latexGenerationData, explicitRefinements)
    documentReport
  }

  private def layoutStringToPaperSize(layout: String): (PaperLayout, LatexFormatter) = {
    layout.toLowerCase match {
      case "a4" => (PaperLayout.A4, new InlineFormatter())
      case "b4" => (PaperLayout.B4, new MarginFormatter())
      case _ => (PaperLayout.A4, new InlineFormatter())
    }
  }
}