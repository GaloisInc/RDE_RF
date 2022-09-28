package Parser

import Parsers.LobotParser.Compile.LobotLexer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class LobotParserTest extends AnyFlatSpec with should.Matchers {

  "LobotParser" should "be able to generate lexer from simple integer" in {
    val input = "int"

    LobotLexer.parse(input) match {
      case Left(value) => assert(false)
      case Right(value) => value.nonEmpty shouldBe (true)
    }
  }

  "LobotParser" should "be able to generate lexer from simple" in {
    val input = "kind"

    LobotLexer.parse(input) match {
      case Left(value) => assert(false)
      case Right(value) => value.nonEmpty shouldBe (true)
    }
  }
}
