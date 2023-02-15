package Cli

import scopt.DefaultOParserSetup

final case class OParserSetup() extends DefaultOParserSetup {
  override def showUsageOnError: Option[Boolean] = Some(true)
}