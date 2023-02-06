package Interpreters

import Utils.{CommandLineTool, FileUtil}
import org.apache.logging.log4j.scala.Logging

import scala.sys.process.ProcessLogger

object LobotInterpreter extends Logging with CommandLineTool{
  override val  command: String = "lobot"
  override val toolName: String = "Lobot"

  // Run Lobot on a given file
  def verifyLobotFile(filePath: String): Boolean = {
    require(filePath.nonEmpty, "filePath must not be empty")
    require(FileUtil.fileExists(filePath), "filePath must exist")
    require(filePath.endsWith(".lobot"), "filePath must end with .lobot")
    require(toolInstalled, "Lobot must be in the path")

    val cmd = command + " " + filePath
    val result = scala.sys.process.Process(cmd).!
    if (result == 0) {
      logger.info("Lobot verified file " + filePath)
      true
    } else {
      logger.error("Lobot could not verify file " + filePath)
      false
    }
  }
}
