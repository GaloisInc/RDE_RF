package EnvironmentChecker

import Interpreters._
import Report.LatexGenerator
import org.apache.logging.log4j.scala.Logging

object EnvironmentChecker extends Logging {

  def dependenciesInstalled: Boolean = {
    if (!SawInterpreter.verifySawInstalled)
      logger.error("SAW is not installed")

    if (!LatexGenerator.checkLatexInPath())
      logger.error("Latex not found in path - please install it for full functionality")

    if (!CryptolInterpreter.ensureCryptolIsInPath)
      logger.error("Cryptol not found in path - please install it for full functionality")

    //if (LandoInterpreter.verifyLandoInPath)
    // logger.error("Lando not found in path - please install it for full functionality")

    if (!LobotInterpreter.verifyLobotInPath)
      logger.error("Lobot not found in path - please install it for full functionality")

    if (!BlueSpecInterpreter.ensureBlueSpecInPath)
      logger.error("Bluespec not found in path - please install it for full functionality")

    LatexGenerator.checkLatexInPath() &&
      CryptolInterpreter.ensureCryptolIsInPath &&
      LandoInterpreter.verifyLandoInPath &&
      LobotInterpreter.verifyLobotInPath
  }

}
