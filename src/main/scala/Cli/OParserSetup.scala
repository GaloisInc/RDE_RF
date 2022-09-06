package Cli

import scopt.DefaultOParserSetup

case class OParserSetup() extends DefaultOParserSetup {
  override def showUsageOnError = Some(true)
}