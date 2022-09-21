package Cli

case class CLIConfig(
                      sourceFolder: String = "source",
                      targetFolder: String = "target",
                      explicitReferences : String = "",
                      generateLatex: Boolean = false,
                      layout: String = "a4",
                      latexTitle: String = "Latex Document",
                      generateRefinementOverview: Boolean = false,
                      verifyCryptol : Boolean = false,
                    )
