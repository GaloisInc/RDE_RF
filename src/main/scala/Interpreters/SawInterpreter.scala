package Interpreters

import Utils.FileUtil
import org.apache.logging.log4j.scala.Logging

import scala.sys.process.ProcessLogger

object SawInterpreter extends Logging {

  private val sawCmd: String = "saw"

  // Check if the Lando executable is available
  def verifySawInstalled: Boolean = {
    val cmd = sawCmd + " -V"
    println("Checking if SAW is installed")
    val result = scala.sys.process.Process(cmd).!(ProcessLogger(_ => ())) // ignore output
    result == 0
  }

  // Run Lando on a given file
  def verifySawFile(filePath: String): Boolean = {
    require(filePath.nonEmpty, "filePath must not be empty")
    require(FileUtil.fileExists(filePath), "filePath must exist")
    require(filePath.endsWith(".saw"), "filePath must end with .saw")
    require(verifySawInstalled, "saw must be installed")

    val cmd = sawCmd + " " + filePath
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
