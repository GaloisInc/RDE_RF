package Cli

import Utils.FileUtil.createDirectory
import scopt.OParser
import java.io.File

/**
 * This class is responsible for parsing the command line arguments.
 * It uses the scopt library to parse the command line arguments.
 */
object CommandLineParser {
  private val builder = OParser.builder[CLIConfig]

  def createParser: OParser[Unit, CLIConfig] = {
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
          opt[String]('a', "author")
            .action((a, c) => c.copy(author = a))
            .text("author is an optional string property that specifies the author of the documentation."),
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
      opt[Unit]('V', "verify")
        .action((_, c) => c.copy(verifySourceFiles = true))
        .text("verify is an optional boolean property that specifies whether to verify the source files.")
      ,
      version('v', "version").text("Prints the version of the tool."),
      help('h', "help").text("prints this usage text"),
    )
  }
}