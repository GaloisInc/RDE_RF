package Interpreters

import Utils.FileUtil
import org.apache.logging.log4j.scala.Logging

import scala.sys.process.ProcessLogger

object LobotInterpreter extends Logging {

  private val lobotCMD: String = "lobot"

  // Check if the Lobot executable is available
  def verifyLobotInPath: Boolean = {
    val cmd = lobotCMD + " -h"
    try {
      val status = scala.sys.process.Process(cmd).!(ProcessLogger(_ => ())) // ignore output
      status == 0
    } catch {
      case _: Throwable => false
    }
  }

  // Run Lobot on a given file
  def verifyLobotFile(filePath: String): Boolean = {
    require(filePath.nonEmpty, "filePath must not be empty")
    require(FileUtil.fileExists(filePath), "filePath must exist")
    require(filePath.endsWith(".lobot"), "filePath must end with .lobot")
    require(verifyLobotInPath, "Lobot must be in the path")

    val cmd = lobotCMD + " " + filePath
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
