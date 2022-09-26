package Interpreters

import Utils.FileUtil
import org.apache.logging.log4j.scala.Logging

import scala.sys.process.ProcessLogger

object LandoInterpreter extends Logging {
  private val landoCmd: String = "lando.sh"

  // Check if the Lando executable is available
  def verifyLandoInPath: Boolean = {
    val cmd = landoCmd + " -h"
    val result = scala.sys.process.Process(cmd).!(ProcessLogger(_ => ())) // ignore output
    result == 0
  }

  // Run Lando on a given file
  def verifyLandoFile(filePath: String): Boolean = {
    require(filePath.nonEmpty, "filePath must not be empty")
    require(FileUtil.fileExists(filePath), "filePath must exist")
    require(filePath.endsWith(".lando"), "filePath must end with .lando")
    require(verifyLandoInPath, "Lando executable must be in path")

    val cmd = landoCmd + " " + filePath
    val result = scala.sys.process.Process(cmd).!
    if(result == 0) {
      logger.info("Lando verified file " + filePath)
      true
    } else {
      logger.error("Lando could not verify file " + filePath)
      false
    }
  }
}
