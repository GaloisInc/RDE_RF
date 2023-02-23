package Interpreters

import Specs.FileSpecs
import Utils.{CommandLineTool, FileUtil}
import org.apache.logging.log4j.scala.Logging

object LobotInterpreter extends Logging with CommandLineTool {
  override def command: String = "lobot"
  override def toolName: String = "Lobot"

  // Run Lobot on a given file
  def verifyLobotFile(filePath: String): Boolean = {
    require(FileSpecs.fileChecks(Set(filePath), Set("lobot")), "The file is not a Lobot file.")
    require(toolInstalled, "Lobot must be in the path")
    val result = runCommand(List(filePath))
    if (result == 0) {
      logger.info("Lobot verified file " + filePath)
    } else {
      logger.error("Lobot could not verify file " + filePath)
    }
    result == 0
  }
}
