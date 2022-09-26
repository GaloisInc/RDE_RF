package EnvironmentChecker

import Interpreters.{CryptolInterpreter, LandoInterpreter, LobotInterpreter}
import Report.LatexGenerator
import org.apache.logging.log4j.scala.Logging

object EnvironmentChecker extends Logging {

  def dependenciesInstalled: Boolean = {
    if(!LatexGenerator.checkLatexInPath())
      logger.error("Latex not found in path - please install it for full functionality")
    if(CryptolInterpreter.ensureCryptolIsInPath)
      logger.error("Cryptol not found in path - please install it for full functionality")

    if(LandoInterpreter.verifyLandoInPath)
      logger.error("Lando not found in path - please install it for full functionality")

    if(LobotInterpreter.verifyLobotInPath)
      logger.error("Lobot not found in path - please install it for full functionality")

    LatexGenerator.checkLatexInPath() && CryptolInterpreter.ensureCryptolIsInPath && LandoInterpreter.verifyLandoInPath && LobotInterpreter.verifyLobotInPath

  }

}
