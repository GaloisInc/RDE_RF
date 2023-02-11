package Report

import Utils.CommandLineTool
import org.apache.logging.log4j.scala.Logging

import java.io.File

/**
 * This class is used to generate a LaTeX document.
 * It uses the latexmk tool to build the document.
 */
object LatexGenerator extends Logging with CommandLineTool {
  override val command = "latexmk -pdf -pdflatex='pdflatex -interaction=nonstopmode -synctex=1 -file-line-error -shell-escape' -use-make"
  override val toolName = "Latex - latexmk (pdflatex)"

  def buildLatexFile(latexFile: File): Unit = {
    val filePath = latexFile.getAbsolutePath
    require(filePath.endsWith(".tex"), s"File $filePath is not a LaTeX file")
    logger.info(s"Building LaTeX file $filePath using $command")
    val exitCode = runCommand(List(s"-output-directory=${latexFile.getParent}", filePath))
    assert(exitCode == 0, s"LaTeX build failed with exit code $exitCode")
    // Clean up auxiliary files
    runCommand(List("-c", filePath))
    logger.info(s"LaTeX build successful")
  }
}












