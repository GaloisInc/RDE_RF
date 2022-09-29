package Cli

case class CLIConfig(
                      sourceFolder: String = "source",
                      targetFolder: String = "target",
                      refinementFile: String = "",
                      excludeFolders: Seq[String] = Seq(),
                      generateLatex: Boolean = false,
                      latexLayout: String = "a4",
                      latexTitle: String = "Latex Document",
                      generateRefinementFile: Boolean = false,
                      verifySourceFiles: Boolean = false,
                    )
