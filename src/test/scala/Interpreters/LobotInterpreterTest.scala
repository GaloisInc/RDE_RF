package Interpreters

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class LobotInterpreterTest extends AnyFlatSpec with should.Matchers {

  "Lobot" should "be be in environment" in {
    LobotInterpreter.verifyLobotInPath should be(true)
  }

  it should "be able to check the lobot file of HARDENS" in {
    val lobotFilePath = getClass.getResource("../lobot/lobot.lobot").getPath
    LobotInterpreter.verifyLobotFile(lobotFilePath) should be(true)
  }

  it should "be able to neglect erroneous lobot file" in {
    val lobotFilePath = getClass.getResource("../lobot/lobotFileWithError.lobot").getPath
    LobotInterpreter.verifyLobotFile(lobotFilePath) should be(false)
  }


}
