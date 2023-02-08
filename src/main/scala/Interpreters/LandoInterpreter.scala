package Interpreters

import Utils.{CommandLineTool, FileUtil}
import org.apache.logging.log4j.scala.Logging

object LandoInterpreter extends Logging with CommandLineTool {
  override val command: String = "lando.sh"
  override def toolName: String = "Lando"

  // Run Lando on a given file
  def verifyLandoFile(filePath: String): Boolean = {
    require(filePath.nonEmpty, "filePath must not be empty")
    require(FileUtil.fileExists(filePath), "filePath must exist")
    require(filePath.endsWith(".lando"), "filePath must end with .lando")
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
