package EnvironmentChecker

import Analyzers.AnalyzerSettings
import org.apache.logging.log4j.scala.Logging

object EnvironmentChecker extends Logging {
  def dependenciesInstalled: Boolean = {
    AnalyzerSettings.supportedTools.foreach(tool => {
      if (!tool.toolInstalled)
        logger.error(s"${tool.toolName} not found in path - please install it for full functionality")
    })
    AnalyzerSettings.supportedTools.forall(_.toolInstalled)
  }

}
