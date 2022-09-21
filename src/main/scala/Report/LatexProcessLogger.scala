package Report

import scala.sys.process.ProcessLogger

class LatexProcessLogger extends ProcessLogger {

  val output: StringBuffer = new StringBuffer(80*15)

  // Ignore
  override def out(s: => String): Unit = ()

  override def err(s: => String): Unit = output.append(s)

  override def buffer[T](f: => T): T = f
}
