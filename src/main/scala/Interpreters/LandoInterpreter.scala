package Interpreters

import Specs.FileSpecs
import Utils.CommandLineTool
import org.apache.logging.log4j.scala.Logging

object LandoInterpreter extends Logging with CommandLineTool {
  def command: String = "lando.sh"
  def toolName: String = "Lando"

  // Run Lando on a given file
  def verifyLandoFile(filePath: String): Boolean = {
    require(FileSpecs.fileChecks(Set(filePath), Set("lando")), "The file is not a lando file.")
    require(toolInstalled, "Lando executable must be in path")

    val result = runCommand(List(filePath))
    if (result == 0) {
      logger.info("Lando verified file " + filePath)
    } else {
      logger.error("Lando could not verify file " + filePath)
    }
    result == 0
  }
}
