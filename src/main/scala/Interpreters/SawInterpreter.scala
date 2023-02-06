package Interpreters

import Utils.{CommandLineTool, FileUtil}
import org.apache.logging.log4j.scala.Logging

import scala.sys.process.ProcessLogger

object SawInterpreter extends Logging with CommandLineTool{
  override val command: String = "saw"
  override val toolName: String = "Saw"

  // Run Lando on a given file
  def verifySawFile(filePath: String): Boolean = {
    require(filePath.nonEmpty, "filePath must not be empty")
    require(FileUtil.fileExists(filePath), "filePath must exist")
    require(filePath.endsWith(".saw"), "filePath must end with .saw")
    require(toolInstalled, "saw must be installed")


    logger.info("Saw checking file " + filePath)
    val cmd = command + " " + filePath
    val output = scala.sys.process.Process(cmd).!!
    logger.info(output)
    val result = scala.sys.process.Process(cmd).!
    if (result == 0) {
      logger.info("SAW verified file " + filePath)
      true
    } else {
      logger.error("SAW could not verify file " + filePath)
      false
    }
  }
}
