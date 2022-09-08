package Cli

import Analyzers.DocumentAnalyzer
import Report.LatexGenerator
import Utils.FileUtil
import org.legogroup.woof.*
import scopt.OParser

import scala.io.StdIn
import scala.util.matching.Regex


object DocumentationEnhancerApp extends App {

  val builder = OParser.builder[CLIConfig]

  val parser = {
    import builder.*
    OParser.sequence(
      programName("DocumentationEnhancer"),
      head("DocumentationEnhancer", "1.0"),
        opt[String] ('s', "sourceFolder")
        .required()
        .action((x, c) => c.copy(sourceFolder = x))
        .text("sourceFolder is a required string property that specifies the folder where the source/code files are located."),
        opt[String] ('t', "targetFolder")
        .required()
        .action((x, c) => c.copy(targetFolder = x))
        .text("targetFolder is a required string property that specifies the folder where the enhanced documentation files are located."),
        opt[Unit] ('l', "generateLatex")
        .action((_, c) => c.copy(generateLatex = true))
        .text("generateLatex is an optional boolean property that specifies whether to generate LaTeX documentation files."),
        opt[String] ('n', "latexTitle")
        .action((x, c) => c.copy(latexTitle = x))
        .text("latexTitle is an optional boolean property that specifies whether to generate LaTeX documentation files. " +
          "If not specified the title of the LaTeX document will be Documentation."),
        help ("help").text("prints this usage text")
    )
  }

  OParser.parse(parser, args, CLIConfig(), OParserSetup()) match {
    case Some(config) =>
      val sourceFolder = config.sourceFolder
      val targetFolder = config.targetFolder
      val generateLatex = config.generateLatex
      val latexTitle = if config.latexTitle.isEmpty then "Documentation" else config.latexTitle
      val fileTypesOfTypesOfInterest = Set("lando", "sysml", "lobot", "cry", "c", "bsv", "sv")

      val files = FileUtil.findSourceFiles(sourceFolder, fileTypesOfTypesOfInterest)

      require(files.nonEmpty, "No files found in source folder: " + sourceFolder)
      val documents = DocumentAnalyzer.generateReport(files, latexTitle, targetFolder)
      println("The files have been enriched and sorted into different folders in the folder " + targetFolder + ".")
      if (generateLatex) {
        LatexGenerator.generateLatexReport(documents)
        println("The LaTeX files have been generated and compiled in the folder " + targetFolder + ".")
      }
      println("Done!")
    case _ =>
      println("Invalid arguments!")
  }

}