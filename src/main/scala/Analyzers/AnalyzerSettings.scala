package Analyzers

import Interpreters.{BlueSpecInterpreter, CryptolInterpreter, LandoInterpreter, LobotInterpreter}
import Report.LatexGenerator

object AnalyzerSettings {
  val supportedDocumentTypesString = Set("sv", "bsv", "lando", "lobot", "sysml", "cry", "saw")

  val supportedDocumentTypes: Set[Types.DocumentType.Value] = Set(Types.DocumentType.SV, Types.DocumentType.BSV, Types.DocumentType.Lando, Types.DocumentType.Lobot, Types.DocumentType.SysML, Types.DocumentType.Cryptol, Types.DocumentType.Saw)

  lazy val dependenciesSupported: installedDependencies = {
    installedDependencies(
      CryptolInterpreter.ensureCryptolIsInPath,
      LandoInterpreter.verifyLandoInPath,
      LobotInterpreter.verifyLobotInPath,
      LatexGenerator.checkLatexInPath(),
      BlueSpecInterpreter.ensureBlueSpecInPath
    )
  }

  final case class installedDependencies(cryptolInstalled: Boolean,
                                         landoInstalled: Boolean,
                                         lobotInstalled: Boolean,
                                         latexInstalled: Boolean,
                                         bluespecInstalled: Boolean)

}
