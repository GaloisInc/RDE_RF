package Utils

import Report.LatexProcessLogger

import scala.sys.process.Process

trait CommandLineTool {
  def command : String
  def toolName : String
  def toolInstalled: Boolean = {
    val path = System.getenv("PATH")
    assert(path != null || path.contains(command), s"$command not installed")
    true
  }

  def runCommand(args: List[String]): Int = {
    val cmd = command :: args
    val cmdString = cmd.mkString(" ")
    val logger = new LatexProcessLogger
    val status = Process(cmdString).!(logger)
    if (status != 0) {
      println(logger.output)
    }
    status
  }

}
