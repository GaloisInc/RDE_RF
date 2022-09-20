package Cli

import Analyzers.{DocumentAnalyzer, LatexDocumentData}
import ConfigParser.{ObjectConfigGenerator, RefinementLoader, RefinementModel}
import Formatter.{InlineFormatter, LatexFormatter, MarginFomatter}
import Report.PaperLayout.PaperLayout
import Report.{LatexGenerator, PaperLayout}
import Utils.FileUtil
import scopt.OParser

import java.io.File


object DocumentationEnhancerApp extends App {
  val fileTypesOfTypesOfInterest = Set("lando", "sysml", "cry", "bsv", "sv")

  val builder = OParser.builder[CLIConfig]

  val parser = {
    import builder._
    OParser.sequence(
      //programName("DocumentationEnhancer"),
      head("DocumentationEnhancer", "1.0"),
      opt[String]('s', "sourceFolder")
        .required()
        .action((x, c) => c.copy(sourceFolder = x))
        .text("sourceFolder is a required string property that specifies the folder where the source/code files are located."),
      opt[String]('t', "targetFolder")
        .required()
        .action((x, c) => c.copy(targetFolder = x))
        .text("targetFolder is a required string property that specifies the folder where the enhanced documentation files are located."),
      opt[String]('c', "refinementConfig")
        .action((x, c) => c.copy(explicitReferences = x))
        .text("refinementConfig is a file containing explicit refinements that should be considered by the tool."),
      opt[Unit]('l', "generateLatex")
        .action((_, c) => c.copy(generateLatex = true))
        .text("generateLatex is an optional boolean property that specifies whether to generate LaTeX documentation files."),
      opt[String]('d', "layoutOfDocumentation")
        .action((x, c) => c.copy(layout = x))
        .text("dimensions is an optional boolean property that specifies the dimensions of the paper, allowed values (a4,b4)."),
      opt[String]('n', "latexTitle")
        .action((x, c) => c.copy(latexTitle = x))
        .text("latexTitle is an optional boolean property that specifies whether to generate LaTeX documentation files. " +
          "If not specified the title of the LaTeX document will be Documentation."),
      opt[Unit]('r', "Generate Refinement Overview")
        .action((_, c) => c.copy(generateRefinementOverview = true))
        .text("showRefinements is an optional boolean property that specifies whether to generate a refinement overview."),
      help("help").text("prints this usage text")
    )
  }

  OParser.parse(parser, args, CLIConfig(), OParserSetup()) match {
    case Some(config) =>
      val sourceFolder = new File(config.sourceFolder).getAbsolutePath
      val targetFolder = new File(config.targetFolder).getAbsolutePath
      val latexTitle = if (config.latexTitle.isEmpty) "Documentation" else config.latexTitle
      val generateRefinementOverview = config.generateRefinementOverview
      val explicitRefinements = config.explicitReferences
      val refinementFile = new File(explicitRefinements)
      val layout = if (config.layout.equalsIgnoreCase("b4") || config.layout.equalsIgnoreCase("a4")) config.layout else "a4"

      val files = FileUtil.findSourceFiles(sourceFolder, fileTypesOfTypesOfInterest)

      require(files.nonEmpty, "No files found in source folder: " + sourceFolder)

      val latexDimensions = layoutStringToPaperSize(layout)

      val latexGenerationData = LatexDocumentData(latexTitle, targetFolder, latexDimensions._1, latexDimensions._2)

      val documentReport = if (refinementFile.exists()) {
        files.foreach(file => println("Processing file: " + file))
        println("Loading explicit refinements from: " + refinementFile.getAbsolutePath)
        val explicitRefinements = RefinementLoader.load(refinementFile.getAbsolutePath).explicit_refinements
        DocumentAnalyzer.generateReport(files, latexGenerationData, explicitRefinements.toSet)
      } else {
        DocumentAnalyzer.generateReport(files, latexGenerationData, Set.empty[RefinementModel])
      }

      println("The files have been enriched and sorted into different folders in the folder " + targetFolder + ".")
      if (config.generateLatex) {
        LatexGenerator.generateLatexReportOfSources(documentReport)
        println("The LaTeX files have been generated and compiled in the folder " + targetFolder + ".")
      }
      if (generateRefinementOverview) {
        ObjectConfigGenerator.generateRefinementConfigFile(documentReport, "refinementOverview")
        println("The refinement overview has been generated in the folder " + targetFolder + ".")
      }
      println("Done!")
      System.exit(0)
    case _ =>
      println("Invalid arguments!")
      System.exit(1)
  }

  def layoutStringToPaperSize(layout: String): (PaperLayout, LatexFormatter) = {
    layout.toLowerCase match {
      case "a4" => (PaperLayout.A4, new InlineFormatter())
      case "b4" => (PaperLayout.B4, new MarginFomatter())
      case _ => (PaperLayout.A4, new InlineFormatter())
    }
  }
}