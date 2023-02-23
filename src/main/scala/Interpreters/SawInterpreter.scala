package Interpreters

import Specs.FileSpecs
import Utils.{CommandLineTool, FileUtil}
import org.apache.logging.log4j.scala.Logging

object SawInterpreter extends Logging with CommandLineTool {
  override def command: String = "saw"
  override def toolName: String = "Saw"

  // Run Lando on a given file
  def verifySawFile(filePath: String): Boolean = {
    require(FileSpecs.fileChecks(Set(filePath), Set("saw")), "The file is not a Saw file.")
    require(toolInstalled, "saw must be installed")

    logger.info("Saw checking file " + filePath)
    val result = runCommand(List(filePath))
    if (result == 0) {
      logger.info("SAW verified file " + filePath)
    } else {
      logger.error("SAW could not verify file " + filePath)
    }
    result == 0
  }
}
