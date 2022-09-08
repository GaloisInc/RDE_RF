package Cli

case class CLIConfig(
                      sourceFolder: String = "source",
                      targetFolder: String = "target",
                      generateLatex: Boolean = false,
                      latexTitle: String = "",
                    )
