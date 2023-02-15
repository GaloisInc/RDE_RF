package Cli

case class CLIConfig(
                      author: String = "Refinement Finder by Galois, Inc.",
                      sourceFolder: String = "source",
                      targetFolder: String = "target",
                      refinementFile: String = "",
                      excludeFolders: Seq[String] = Seq(),
                      generateLatex: Boolean = false,
                      latexLayout: String = "a4",
                      latexTitle: String = "Documentation",
                      generateRefinementFile: Boolean = false,
                      verifySourceFiles: Boolean = false,
                      deleteDecoratedFiles: Boolean = false
                    )
