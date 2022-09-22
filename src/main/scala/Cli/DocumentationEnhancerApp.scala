package Cli

import Analyzers.{DocumentAnalyzer, LatexDocumentData}
import ConfigParser.{ObjectConfigGenerator, RefinementLoader, RefinementModel}
import Formatter.{InlineFormatter, LatexFormatter, MarginFomatter}
import Interpreters.CryptolInterpreter
import Report.PaperLayout.PaperLayout
import Report.ReportTypes.ReportReference
import Report.{LatexGenerator, PaperLayout}
import Utils.FileUtil
import scopt.OParser

import java.io.File


object DocumentationEnhancerApp extends App {
  val fileTypesOfTypesOfInterest = Set("lando", "sysml", "cry", "bsv", "sv")

  val builder = OParser.builder[CLIConfig]

  val parser = {
    //Command line GNU
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
          else failure("Output folder does not exist"))
        .text("outputFolder is a required string property that specifies the folder where the enhanced documentation files should be placed."),
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
      opt[Unit]('R', "Generate Refinement Overview")
        .action((_, c) => c.copy(generateRefinementFile = true))
        .text("showRefinements is an optional boolean property that specifies whether to generate a refinement overview.")
      ,
      opt[Unit]('A', "verifyCryptolSpecifications")
        .action((_, c) => c.copy(verifyCryptol = true))
        .text("verifyCryptolSpecifications is an optional boolean property that specifies whether to verify the Cryptol specifications." +
          "This requires the Cryptol executable to be in the PATH.")
      ,
      version('v', "version").text("Prints the version of the tool."),
      help('h', "help").text("prints this usage text")
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

      if (files.isEmpty) {
        println("No files found in source folder: " + sourceFolder)
        System.exit(1)
      }

      verifyCryptolDocuments(config, sourceFolder)

      val latexDimensions = layoutStringToPaperSize(layout)

      val latexGenerationData = LatexDocumentData(latexTitle, targetFolder, latexDimensions._1, latexDimensions._2)

      val documentReport: ReportReference = generateReport(refinementFile, files, latexGenerationData)

      println("The files have been enriched and sorted into different folders in the folder " + targetFolder + ".")
      if (config.generateLatex) {
        LatexGenerator.generateLatexReportOfSources(documentReport)
        println("The LaTeX files have been generated and compiled in the folder " + targetFolder + ".")
      }
      if (config.generateRefinementFile) {
        ObjectConfigGenerator.generateRefinementConfigFile(documentReport, "refinementOverview")
        println("The refinement overview has been generated in the folder " + targetFolder + ".")
      }
      println("Done!")
      System.exit(0)
    case _ =>
      println("Invalid arguments!")
      System.exit(1)
  }

  private def verifyCryptolDocuments(config: CLIConfig, sourceFolder: String): Unit = {
    if (config.verifyCryptol) {
      val cryptolFiles = FileUtil.findSourceFiles(sourceFolder, Set("cry"))
      assert(CryptolInterpreter.ensureCryptolIsInPath, "Cryptol executable not found in PATH. Please install Cryptol and add it to the PATH.")
      if (cryptolFiles.forall(CryptolInterpreter.verifyProperties)) {
        println("All Cryptol specifications verified successfully.")
      } else {
        println("Cryptol specifications could not be verified.")
        System.exit(1)
      }
    }
  }

  private def generateReport(refinementFile: File, files: Array[String], latexGenerationData: LatexDocumentData): ReportReference = {
    val documentReport = if (refinementFile.exists()) {
      files.foreach(file => println("Processing file: " + file))
      println("Loading explicit refinements from: " + refinementFile.getAbsolutePath)
      val explicitRefinements = RefinementLoader.load(refinementFile.getAbsolutePath).explicit_refinements
      DocumentAnalyzer.generateReport(files.toSet, latexGenerationData, explicitRefinements.values.flatten.toSet)
    } else {
      DocumentAnalyzer.generateReport(files.toSet, latexGenerationData, Set.empty[RefinementModel])
    }
    documentReport
  }

  def layoutStringToPaperSize(layout: String): (PaperLayout, LatexFormatter) = {
    layout.toLowerCase match {
      case "a4" => (PaperLayout.A4, new InlineFormatter())
      case "b4" => (PaperLayout.B4, new MarginFomatter())
      case _ => (PaperLayout.A4, new InlineFormatter())
    }
  }
}