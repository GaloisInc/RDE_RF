package Cli

import Analyzers.{DocumentAnalyzer, LatexDocumentData, SourceVerifier}
import ConfigParser.{ConfigGenerator, RefinementLoader, RefinementModel}
import EnvironmentChecker.EnvironmentChecker
import Formatter.{InlineFormatter, LatexFormatter, MarginFormatter}
import Report.PaperLayout
import Report.PaperLayout.PaperLayout
import Report.ReportTypes.ReportReference
import Utils.FileUtil
import Utils.FileUtil.createDirectory
import org.apache.logging.log4j.scala.Logging
import scopt.OParser

import java.io.File

object DocumentationEnhancerApp extends App with Logging {
  private val fileTypesOfTypesOfInterest = Analyzers.AnalyzerSettings.supportedDocumentTypesString
  private val builder = OParser.builder[CLIConfig]

  val parser = {
    //Command line GNU style parser
    import builder._
    OParser.sequence(
      programName("DocumentationEnhancer"),
      head("DocumentationEnhancer", "1.0"),
      opt[String]('i', "inputFolder")
        .required()
        .valueName("<folder>")
        .action((x, c) => c.copy(sourceFolder = x))
        .validate(x =>
          if (new File(x).isDirectory) success
          else failure("Input folder with the source files does not exist"))
        .text("inputFolder is a required string property that specifies the folder where the source/code files are located."),
      opt[String]('o', "outputFolder")
        .required()
        .valueName("<folder>")
        .action((x, c) => c.copy(targetFolder = x))
        .validate(x =>
          if (new File(x).isDirectory) success
          else {
            println("Output folder does not exist yet. Creating it now.")
            createDirectory(x)
            success
          }).text("outputFolder is a required string property that specifies the folder where the enhanced documentation files should be placed."),
      opt[Seq[String]]('e', "excludeFolders")
        .valueName("<folder1>,<folder2>,...")
        .action((x, c) => c.copy(excludeFolders = x))
        .text("folders to exclude in the analysis and report."),
      opt[String]('f', "configFile")
        .valueName("<file.conf>")
        .action((x, c) => c.copy(refinementFile = x))
        .validate(x =>
          if (new File(x).isFile) success
          else failure("Config file does not exist"))
        .text("configFile is a an optional string property that specifies the file where the configuration is located."),
      opt[Unit]('g', "generateLatex")
        .action((_, c) => c.copy(generateLatex = true))
        .text("generateLatex is a command that generates a pdf document from the source files.")
        .children(
          opt[String]('d', "layoutOfDocumentation")
            .action((x, c) => c.copy(latexLayout = x))
            .text("layoutOfDocumentation is an optional boolean property that specifies the dimensions of the paper, allowed values (a4,b4)."),
          opt[String]('t', "latexTitle")
            .action((x, c) => c.copy(latexTitle = x))
            .text("latexTitle is an optional string property that specifies whether to generate LaTeX documentation files. " +
              "If not specified the title of the LaTeX document will be Documentation.")
        ),
      opt[Unit]('D', "delete decorated files")
        .action((_, c) => c.copy(deleteDecoratedFiles = true))
        .text("deleteDecoratedFiles is a command that deletes the decorated before generating the latex documentation."),

      opt[Unit]('R', "Generate Refinement Overview")
        .action((_, c) => c.copy(generateRefinementFile = true))
        .text("showRefinements is an optional boolean property that specifies whether to generate a refinement overview.")
      ,
      opt[Unit]('A', "verifyAll")
        .action((_, c) => c.copy(verifySourceFiles = true))
        .text("verifyAll is an optional boolean property that specifies whether to verify all source files.")
      ,
      //opt[String]('r', "gitRepo")
      //  .action((x, c) => c.copy(gitRepo = x))
      //  .text("gitRepo is an optional string property that specifies the git repository to clone."),
      version('v', "version").text("Prints the version of the tool."),
      help('h', "help").text("prints this usage text"),
    )
  }

  OParser.parse(parser, args, CLIConfig(), OParserSetup()) match {
    case Some(config) =>
      val sourceFolder = new File(config.sourceFolder).getAbsolutePath
      val targetFolder = new File(config.targetFolder).getAbsolutePath
      val latexTitle = if (config.latexTitle.isEmpty) "Documentation" else config.latexTitle
      val explicitRefinements = config.refinementFile
      val refinementFile = new File(explicitRefinements)
      val layout = if (config.latexLayout.equalsIgnoreCase("b4") || config.latexLayout.equalsIgnoreCase("a4")) config.latexLayout else "a4"
      val files = FileUtil.findSourceFiles(sourceFolder, fileTypesOfTypesOfInterest)
      val excludedFolders = config.excludeFolders
      val filteredFiles = files.filterNot(file => excludedFolders.exists(folder => file.contains(folder)))
      println("Starting Documentation Enhancer")

      if (config.deleteDecoratedFiles) {
        println("Deleting decorated files")
        FileUtil.deleteRecursivelyDecoratedFiles(targetFolder)
      }

      if (filteredFiles.isEmpty) {
        println("No files found in source folder: " + sourceFolder)
        System.exit(1)
      }

      if (FileUtil.allFilesReadable(filteredFiles)) {
        println("All files are readable")
      } else {
        println("Not all files are readable")
        println("The following files are not readable:" + FileUtil.getNonReadableFiles(filteredFiles).mkString(","))
        filteredFiles.foreach(f => new File(f).setReadable(true))
      }

      verifySourceFiles(config, filteredFiles)

      val latexDimensions = layoutStringToPaperSize(layout)

      val latexGenerationData = LatexDocumentData(latexTitle, "Refinement Finder by Galois, Inc", targetFolder, latexDimensions._1, latexDimensions._2)

      val documentReport: ReportReference = generateReport(refinementFile, filteredFiles, latexGenerationData)

      println("The files have been enriched and sorted into different folders in the folder " + targetFolder + ".")
      if (config.generateLatex) {
        println("You have chosen to generate a LaTeX document. " +
          "The document will be generated in the folder " + targetFolder + ".")

        documentReport.buildDocumentationReport
        println("The LaTeX files have been generated and compiled in the folder " + targetFolder + ".")

      }
      if (config.generateRefinementFile) {
        ConfigGenerator.generateRefinementConfigFile(documentReport, "refinementOverview")
        println("The refinement overview has been generated in the folder " + targetFolder + ".")
      }
      println("Done!")
      System.exit(0)
    case _ =>
      println("Invalid arguments! Please check the usage and try again.")
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
    require(files.nonEmpty, "No files found in source folder")
    require(files.forall(f => {
      assert(fileTypesOfTypesOfInterest.contains(FileUtil.getFileType(f)), "File type not supported: " + f)
      true
    }), "File type not supported")
    require(files.forall(f => {
      assert(FileUtil.fileExists(f), "File does not exist: " + f)
      true
    }), "File does not exist")

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