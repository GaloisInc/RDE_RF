package Interpreters

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class LobotInterpreterTest extends AnyFlatSpec with should.Matchers {
  "Lobot" should "be be in environment" in {
    LobotInterpreter.toolInstalled should be(true)
  }

  it should "be able to check the lobot file of HARDENS" in {
    require(LobotInterpreter.toolInstalled, "Lobot not installed. Please install Lobot")
    val lobotFilePath = getClass.getResource("../lobot/lobot.lobot").getPath
    LobotInterpreter.verifyLobotFile(lobotFilePath) should be(true)
  }

  it should "be able to neglect erroneous lobot file" in {
    require(LobotInterpreter.toolInstalled, "Lobot not installed. Please install Lobot")
    val lobotFilePath = getClass.getResource("../lobot/lobotFileWithError.lobot").getPath
    LobotInterpreter.verifyLobotFile(lobotFilePath) should be(false)
  }
}
