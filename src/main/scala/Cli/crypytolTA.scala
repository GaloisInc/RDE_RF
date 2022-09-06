package Cli

import java.io.{BufferedWriter, File, FileWriter, IOException}


import scala.sys.process._

object CrypytolTA {
  val CRYPTOL = "cryptol"

  def checkEnvironment(): Boolean = {
    //logger.info("Checking for binary in system.")
    try {
      val pLog = new VerifyTaProcessLogger()
      val cmd = CRYPTOL
      val exitCode = Process(cmd).!(pLog)

      if (exitCode != 0) {
        //logger.error(s"Command failed: $cmd")
        false
      } else {
        val versionOut = pLog.output.toString
        //logger.debug(s"${CRYPTOL} version:")
        //logger.debug(versionOut)
        true
      }
    } catch {
      case e: IOException => {
//        logger.error("Problem with Cryptol. Make sure it is in the PATH.")
//        logger.info("Current PATH:")
//        logger.info(System.getenv("PATH"))
//        logger.info("Underlying exception was:")
//        logger.info(e)
        false
      }
    }
  }
}