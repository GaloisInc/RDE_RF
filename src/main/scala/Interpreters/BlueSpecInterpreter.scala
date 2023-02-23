package Interpreters

import Specs.FileSpecs
import Utils.{CommandLineTool, FileUtil}
import org.apache.logging.log4j.scala.Logging

object BlueSpecInterpreter extends Logging with CommandLineTool {
  def command: String = "bsc"
  def toolName: String = "BlueSpecVerilog"

  def isWellFormed(filePath: String): Boolean = {
    bscCheck(filePath)
    logger.info("BlueSpec checking file " + filePath)
    if (runCommand(List(filePath)) == 0) {
      logger.info(s"The file $filePath is well-formed.")
      true
    } else {
      logger.info(s"The file $filePath is not well-formed.")
      false
    }
  }

  private def bscCheck(filePath: String): Unit = {
    require(FileSpecs.fileChecks(Set(filePath), Set("bsv")), "The file is not a BlueSpec file.")
    require(FileUtil.fileExists(filePath), "The file does not exist.")
  }

  private def generateFile(filePath: String, flag: String, fileEnding: String): String = {
    bscCheck(filePath)
    logger.info("BlueSpec generating file " + filePath)
    val result = runCommand(List(flag, filePath))
    if (result == 0) {
      logger.info(s"Successfully generated file for $filePath.")
      filePath.replace(".bsv", fileEnding)
    } else {
      logger.info(s"Failed to generate file for $filePath.")
      ""
    }
  } ensuring(FileUtil.fileExists(_), "The file does not exist.")

  def generateVerilogFile(filePath: String): String = {
    logger.info("BlueSpec generating verilog file " + filePath)
    generateFile(filePath, "-u", ".v")
  } ensuring(FileUtil.fileExists(_), "The file does not exist.")

  def generateBluesimObject(filePath: String): String = {
    bscCheck(filePath)
    logger.info("BlueSpec generating bluesim object " + filePath)
    generateFile(filePath, "-sim", ".bo")
  } ensuring(FileUtil.fileExists(_), "The file does not exist.")
}
