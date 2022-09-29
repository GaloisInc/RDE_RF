package Interpreters

import Utils.FileUtil
import org.apache.logging.log4j.scala.Logging

import scala.sys.process._

object BlueSpecInterpreter extends Logging {
  private val blueSpecCmd: String = "bsc"

  def isWellFormed(filePath: String): Boolean = {
    require(filePath.nonEmpty, "Filename is not specified.")
    require(filePath.endsWith(".bsv"), "The file is not a cryptol file.")
    require(ensureBlueSpecInPath, "BlueSpec is not in the path.")
    require(FileUtil.fileExists(filePath), "The file does not exist.")

    logger.info("BlueSpec checking file " + filePath)
    val result = s"$blueSpecCmd $filePath".!
    if (result == 0) {
      logger.info(s"The file $filePath is well-formed.")
      true
    } else {
      logger.info(s"The file $filePath is not well-formed.")
      false
    }
    result == 0
  }

  def ensureBlueSpecInPath: Boolean = {
    val status = s"$blueSpecCmd -help".!(ProcessLogger(_ => ())) // ignore output
    status == 0
  }

  def bscCheck(filePath: String) : Unit = {
    require(filePath.nonEmpty, "Filename is not specified.")
    require(filePath.endsWith(".bsv"), "The file is not a cryptol file.")
    require(ensureBlueSpecInPath, "BlueSpec is not in the path.")
    require(FileUtil.fileExists(filePath), "The file does not exist.")
  }

  def generateVerilogFile(filePath: String): String = {
    bscCheck(filePath)

    val result = s"$blueSpecCmd -verilog -u $filePath".!
    if (result == 0) {
      logger.info(s"Successfully generated verilog file for $filePath.")
      filePath.replace(".bsv", ".v")
    } else {
      logger.info(s"Failed to generate verilog file for $filePath.")
      ""
    }
  } ensuring(FileUtil.fileExists(_), "The file does not exist.")

  def generateBluesimObject(filePath: String): String = {
    require(filePath.nonEmpty, "Filename is not specified.")
    require(filePath.endsWith(".bsv"), "The file is not a cryptol file.")
    require(ensureBlueSpecInPath, "BlueSpec is not in the path.")
    require(FileUtil.fileExists(filePath), "The file does not exist.")

    val result = s"$blueSpecCmd -sim $filePath".!
    if (result == 0) {
      logger.info(s"Successfully generated bluesim object for $filePath.")
      filePath.replace(".bsv", ".bo")
    } else {
      logger.info(s"Failed to generate bluesim object for $filePath.")
      ""
    }
  } ensuring(FileUtil.fileExists(_), "The file does not exist.")


}
