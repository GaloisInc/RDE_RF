package Analyzers

import EnvironmentChecker.EnvironmentChecker
import Interpreters.{BlueSpecInterpreter, CryptolInterpreter, LobotInterpreter, SawInterpreter}
import Utils.FileUtil
import org.apache.logging.log4j.scala.Logging

object SourceVerifier extends Logging {
  def verifySourceFiles(sourceFiles: Array[String]): List[String] = {
    require(sourceFiles.forall(file => {
      val fileExists = FileUtil.fileExists(file)
      if (!fileExists) logger.error(s"File $file does not exist")
      fileExists
    }), "One or more source files do not exist")
    require(EnvironmentChecker.dependenciesInstalled, "Environment is not set up correctly - please install dependencies.")

    val filesWithCompilationError = sourceFiles.filterNot(file => isWellFormed(file)).toList

    filesWithCompilationError
  }

  def isWellFormed(filePath: String): Boolean = {
    FileUtil.getFileType(filePath) match {
      case "cry" => CryptolInterpreter.verifyProperties(filePath)
      case ".icry" => CryptolInterpreter.runVerificationScript(filePath)
      case "saw" => SawInterpreter.verifySawFile(filePath)
      case "lando" => true //LandoInterpreter.verifyLandoFile(filePath)
      case "lobot" => LobotInterpreter.verifyLobotFile(filePath)
      case "sysml" => true //We don't have a sysml interpreter yet
      case "bsv" => BlueSpecInterpreter.isWellFormed(filePath)
      case "sv" => true //We don't have a sv interpreter yet
      case _ => throw new IllegalArgumentException(s"File type not supported: $filePath")
    }
  }
}
