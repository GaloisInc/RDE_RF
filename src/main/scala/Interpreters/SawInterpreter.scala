package Interpreters

import Utils.{CommandLineTool, FileUtil}
import org.apache.logging.log4j.scala.Logging

object SawInterpreter extends Logging with CommandLineTool {
  override val command: String = "saw"
  override val toolName: String = "Saw"

  // Run Lando on a given file
  def verifySawFile(filePath: String): Boolean = {
    require(filePath.nonEmpty, "filePath must not be empty")
    require(FileUtil.fileExists(filePath), "filePath must exist")
    require(filePath.endsWith(".saw"), "filePath must end with .saw")
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
