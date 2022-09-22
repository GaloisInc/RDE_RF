package Analyzers

import Formatter.LatexFormatter
import Report.PaperLayout.PaperLayout

final case class LatexDocumentData(
                                    title: String,
                                    folder: String,
                                    layout: PaperLayout,
                                    latexFormatter: LatexFormatter
                                  ) {
  require(title.nonEmpty, "Title must not be empty")
  require(folder.nonEmpty, "Folder must not be empty")
}
