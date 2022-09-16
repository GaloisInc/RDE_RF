package Cli

import Analyzers.{DocumentAnalyzer, LatexDocumentData}
import Formatter.{InlineFormatter, LatexFormatter, MarginFomatter}
import Report.PaperLayout.PaperLayout
import Report.{LatexGenerator, PaperLayout, RefinementReport}
import Utils.FileUtil
import scopt.OParser


object DocumentationEnhancerApp extends App {
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
      opt[Unit]('r', "showRefinements")
        .action((_, c) => c.copy(showRefinement = true))
        .text("Generate Refinement Chains in the documentation."),
      help("help").text("prints this usage text")
    )
  }

  OParser.parse(parser, args, CLIConfig(), OParserSetup()) match {
    case Some(config) =>
      val sourceFolder = config.sourceFolder
      val targetFolder = config.targetFolder
      val generateLatex = config.generateLatex
      val latexTitle = if (config.latexTitle.isEmpty) "Documentation" else config.latexTitle
      val showRefinements = config.showRefinement
      val layout = if (config.layout.equalsIgnoreCase("b4") || config.layout.equalsIgnoreCase("a4")) config.layout else "a4"
      val fileTypesOfTypesOfInterest = Set("lando", "sysml", "lobot", "cry", "c", "bsv", "sv")

      val files = FileUtil.findSourceFiles(sourceFolder, fileTypesOfTypesOfInterest)

      require(files.nonEmpty, "No files found in source folder: " + sourceFolder)

      val latexDimensions = layoutStringToPaperSize(layout)

      val latexGenerationData = LatexDocumentData(latexTitle, targetFolder, latexDimensions._1, latexDimensions._2)

      val documents = DocumentAnalyzer.generateReport(files, latexGenerationData)
      println("The files have been enriched and sorted into different folders in the folder " + targetFolder + ".")
      if (generateLatex) {
        LatexGenerator.generateLatexReportOfSources(documents)
        println("The LaTeX files have been generated and compiled in the folder " + targetFolder + ".")
      }
      if (showRefinements) {
        RefinementReport.generateRefinementReport(documents)
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