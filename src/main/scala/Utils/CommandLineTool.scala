package Utils

import Report.LatexProcessLogger
import org.apache.logging.log4j.scala.Logging
import scala.sys.process.Process

trait CommandLineTool extends Logging {
  def command: String

  def toolName: String

  //require(command.nonEmpty, "command must not be empty")
  //require(toolName.nonEmpty, "toolName must not be empty")

  def toolInstalled: Boolean = {
    val path = System.getenv("PATH")
    assert(path != null || path.contains(command), s"$command not installed")
    true
  }

  def runCommand(args: List[String]): Int = {
    runCommand(command, args)
  }

  def runCommand(requested_command : String, args: List[String]): Int = {
    val cmd = requested_command :: args
    val cmdString = cmd.mkString(" ")
    logger.info(s"Running $toolName with command: $cmdString")
    val processLogger = new LatexProcessLogger
    val status = Process(cmdString).!(processLogger)
    if (status != 0) {
      logger.error(s"Command $cmdString failed with exit code $status")
      println(processLogger.output)
    }
    status
  }

}
