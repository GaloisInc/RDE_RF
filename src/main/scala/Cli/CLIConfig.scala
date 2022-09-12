package Cli

case class CLIConfig(
                      sourceFolder: String = "source",
                      targetFolder: String = "target",
                      generateLatex: Boolean = false,
                      layout: String = "a4",
                      latexTitle: String = "",
                      showRefinement: Boolean = false,
                    )
