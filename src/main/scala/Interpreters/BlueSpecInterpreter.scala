package Interpreters

import Utils.{CommandLineTool, FileUtil}
import org.apache.logging.log4j.scala.Logging

import scala.sys.process._

object BlueSpecInterpreter extends Logging with CommandLineTool {
  override val command: String = "bsc"
  override val toolName: String = "BlueSpecVerilog"

  def isWellFormed(filePath: String): Boolean = {
    require(filePath.nonEmpty, "Filename is not specified.")
    require(filePath.endsWith(".bsv"), "The file is not a cryptol file.")
    require(toolInstalled, "BlueSpec is not in the path.")
    require(FileUtil.fileExists(filePath), "The file does not exist.")

    logger.info("BlueSpec checking file " + filePath)
    val result = s"$command $filePath".!
    if (result == 0) {
      logger.info(s"The file $filePath is well-formed.")
      true
    } else {
      logger.info(s"The file $filePath is not well-formed.")
      false
    }
    result == 0
  }
  def bscCheck(filePath: String) : Unit = {
    require(filePath.nonEmpty, "Filename is not specified.")
    require(filePath.endsWith(".bsv"), "The file is not a cryptol file.")
    require(toolInstalled, "BlueSpec is not in the path.")
    require(FileUtil.fileExists(filePath), "The file does not exist.")
  }

  def generateVerilogFile(filePath: String): String = {
    bscCheck(filePath)

    val result = s"$command -verilog -u $filePath".!
    if (result == 0) {
      logger.info(s"Successfully generated verilog file for $filePath.")
      filePath.replace(".bsv", ".v")
    } else {
      logger.info(s"Failed to generate verilog file for $filePath.")
      ""
    }
  } ensuring(FileUtil.fileExists(_), "The file does not exist.")

  def generateBluesimObject(filePath: String): String = {
    bscCheck(filePath)

    val result = s"$command -sim $filePath".!
    if (result == 0) {
      logger.info(s"Successfully generated bluesim object for $filePath.")
      filePath.replace(".bsv", ".bo")
    } else {
      logger.info(s"Failed to generate bluesim object for $filePath.")
      ""
    }
  } ensuring(FileUtil.fileExists(_), "The file does not exist.")


}
