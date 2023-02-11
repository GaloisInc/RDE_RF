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
    require(FileUtil.fileExists(filePath), s"File $filePath does not exist")
    require(EnvironmentChecker.dependenciesInstalled, "Environment is not set up correctly - please install dependencies.")
    FileUtil.getDocumentType(filePath) match {
      case Types.DocumentType.Lando => true //LandoInterpreter.verifyLandoFile(filePath)
      case Types.DocumentType.Lobot => LobotInterpreter.verifyLobotFile(filePath)
      case Types.DocumentType.SysML => true //We don't have a sysml interpreter yet
      case Types.DocumentType.Cryptol => CryptolInterpreter.verifyProperties(filePath)
      case Types.DocumentType.Saw => SawInterpreter.verifySawFile(filePath)
      case Types.DocumentType.SV => true //We don't have a sv interpreter yet
      case Types.DocumentType.BSV => BlueSpecInterpreter.isWellFormed(filePath)
      case Types.DocumentType.C => true //We don't have a C interpreter yet
      case Types.DocumentType.Fret => true //We don't have a Fret interpreter yet
    }
  }
}