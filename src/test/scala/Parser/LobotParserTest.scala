package Parser

import Parsers.LobotParser.Compile.LobotLexer
import Parsers.LobotParser.Models.{IDENTIFIER, INT_LITERAL, KIND, TYPE}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class LobotParserTest extends AnyFlatSpec with should.Matchers {

  "LobotParser" should "be able to generate lexer from simple kindDecl" in {
    val input = "nat : kind of int where self >= 0"

    LobotLexer.parse(input) match {
      case Left(value) => assert(false)
      case Right(value) => value.head shouldBe(IDENTIFIER("nat"))
    }
  }

  "LobotParser" should "be able to generate lexer from simple type" in {
    val input = "type dev_board =\n  { Virtual, LFE5UM5G_85F_EVN, RV32M1_VEGA, None }"

    LobotLexer.parse(input) match {
      case Left(value) => assert(false)
      case Right(value) => value.head shouldBe(TYPE())
    }
  }

  "LobotParser" should "be able to generate lexer from check decl type" in {
    val input = "twin_build_configs : check\n  on c : virtualized_rts_configs\n  that c.board = None"

    LobotLexer.parse(input) match {
      case Left(value) => assert(false)
      case Right(value) => value.head shouldBe(IDENTIFIER("twin_build_configs"))
      value.size shouldBe 13
    }
  }

  "LobotParser" should "be able to generate lexer from struct Kind type" in {
    val input = "rts : kind of struct\n  with      board : dev_board"

    LobotLexer.parse(input) match {
      case Left(value) => println(value)
      case Right(value) => value.head shouldBe(IDENTIFIER("rts"))
      value.size shouldBe 9
    }
  }

  "LobotParser" should "be able to generate lexer from struct decl type" in {
    val input = "virtualized_rts_configs : kind of rts\n  where all_devices_twins = true & cost = 0 & board = None & virtualized_platform_rt = true"

    LobotLexer.parse(input) match {
      case Left(value) => assert(false)
      case Right(value) => value.head shouldBe(IDENTIFIER("virtualized_rts_configs"))
        value.size shouldBe 21
    }
  }


}
