package Analyzers

import Interpreters.{BlueSpecInterpreter, CryptolInterpreter, LandoInterpreter, LobotInterpreter}
import Report.LatexGenerator
import Utils.CommandLineTool

object AnalyzerSettings {
  val supportedTools : Set[CommandLineTool] = Set(CryptolInterpreter, LandoInterpreter, LobotInterpreter, LatexGenerator, BlueSpecInterpreter)

  val supportedDocumentTypesString: Set[String] = Set("sv", "bsv", "lando", "lobot", "sysml", "cry", "saw", "c", "h")

  val supportedDocumentTypes: Set[Types.DocumentType.Value] = Set(Types.DocumentType.SV, Types.DocumentType.BSV, Types.DocumentType.Lando,
    Types.DocumentType.Lobot, Types.DocumentType.SysML, Types.DocumentType.Cryptol, Types.DocumentType.Saw, Types.DocumentType.C)

  lazy val dependenciesSupported: installedDependencies = {
    installedDependencies(
      CryptolInterpreter.toolInstalled,
      LandoInterpreter.toolInstalled,
      LobotInterpreter.toolInstalled,
      LatexGenerator.toolInstalled,
      BlueSpecInterpreter.toolInstalled
    )
  }

  final case class installedDependencies(cryptolInstalled: Boolean,
                                         landoInstalled: Boolean,
                                         lobotInstalled: Boolean,
                                         latexInstalled: Boolean,
                                         bluespecInstalled: Boolean)

}
