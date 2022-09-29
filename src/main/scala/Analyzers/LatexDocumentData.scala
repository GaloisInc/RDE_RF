package Analyzers

import Formatter.LatexFormatter
import Report.PaperLayout
import Report.PaperLayout.PaperLayout

import java.io.File

final case class LatexDocumentData(
                                    title: String,
                                    folder: String,
                                    layout: PaperLayout,
                                    latexFormatter: LatexFormatter
                                  ) {
  require(title.nonEmpty, "Title must not be empty")
  require(folder.nonEmpty, "Folder must not be empty")

  require(layout == PaperLayout.A4 || layout == PaperLayout.B4, "Layout must be A4 or B4")
  require(latexFormatter != null, "LatexFormatter must not be null")

  if (new File(folder).exists()) {
    require(new File(folder).isDirectory, "Folder must be a directory")
  } else {
    new File(folder).mkdirs()
  }
}
