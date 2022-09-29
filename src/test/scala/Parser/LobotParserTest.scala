package Parser

import Parsers.LobotParser.Compile.{LobotLexer, ParserTop}
import Parsers.LobotParser.Models.{IDENTIFIER, TYPE}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class LobotParserTest extends AnyFlatSpec with should.Matchers {

  "LobotParser" should "be able to generate lexer from simple kindDecl" in {
    val input = "nat : kind of int where self >= 0"

    LobotLexer.parse(input) match {
      case Left(value) => assert(false)
      case Right(value) => value.head shouldBe (IDENTIFIER("nat"))
    }
  }

  "LobotParser" should "be able to generate lexer from simple type" in {
    val input = "type dev_board =\n  { Virtual, LFE5UM5G_85F_EVN, RV32M1_VEGA, None }"

    LobotLexer.parse(input) match {
      case Left(value) => assert(false)
      case Right(value) => value.head shouldBe (TYPE())
    }
  }

  "LobotParser" should "be able to generate lexer from check decl type" in {
    val input = "twin_build_configs : check\n  on c : virtualized_rts_configs\n  that c.board = None"

    LobotLexer.parse(input) match {
      case Left(value) => assert(false)
      case Right(value) => value.head shouldBe (IDENTIFIER("twin_build_configs"))
        value.size shouldBe 15
    }
  }

  "LobotParser" should "be able to generate lexer from struct Kind type" in {
    val input = "rts : kind of struct\n  with      board : dev_board"

    LobotLexer.parse(input) match {
      case Left(value) => println(value)
      case Right(value) => value.head shouldBe (IDENTIFIER("rts"))
        value.size shouldBe 10
    }
  }

  "LobotParser" should "be able to generate lexer from struct decl type" in {
    val input = "virtualized_rts_configs : kind of rts\n  where all_devices_twins = true & cost = 0 & board = None & virtualized_platform_rt = true"

    LobotLexer.parse(input) match {
      case Left(value) => assert(false)
      case Right(value) => value.head shouldBe (IDENTIFIER("virtualized_rts_configs"))
        value.size shouldBe 22
    }
  }

  "LobotParser" should "be able to generate AST from simple type" in {
    val input = "type dev_board =\n  { Virtual, LFE5UM5G_85F_EVN, RV32M1_VEGA, None }\n"
    val parser = new ParserTop

    val x = for {
      tokens ← LobotLexer.parse(input).right
      ast ← parser(tokens).right
    } yield (tokens, ast)

    x match {
      case Left(value) => assert(false)
      case Right(value) => println(value._2)
    }
  }

  "LobotParser" should "be able to generate AST from simple typeDecl" in {
    val input = "type virtualized_platform_runtime = {Posix, RV32_bare_metal, None}\n"
    val parser = new ParserTop

    println(LobotLexer.parse(input).right)
    val x = for {
      tokens ← LobotLexer.parse(input).right
      ast ← parser(tokens).right
    } yield (tokens, ast)

    x match {
      case Left(value) => assert(false)
      case Right(value) => println(value._2)
    }
  }


  "LobotParser" should "be able to generate AST from simple kindType" in {
    val input = "virtualized_rts_configs : kind of rts\n  where cost >= 0 & all_devices_twins = true & board = None & virtualized_platform_rt = true"
    val parser = new ParserTop


    val x = for {
      tokens ← LobotLexer.parse(input).right
      ast ← parser(tokens).right
    } yield (tokens, ast)

    x match {
      case Left(value) => println(value)
      case Right(value) =>
        println("AST:" + value._2)
        println("Tokens:" + value._1)
    }
  }

  "LobotParser" should "be able to generate AST from checkDecl" in {
    val input = "twin_build_configs : check \n on c : virtualized_rts_configs  that c.board = None"
    val parser = new ParserTop

    val x = for {
      tokens ← LobotLexer.parse(input).right
      ast ← parser(tokens).right
    } yield (tokens, ast)

    x match {
      case Left(value) => println(value)
      case Right(value) =>
        val ast = value._2
        println("AST:" + ast)
        println("Tokens:" + value._1)
    }
  }

  "LobotParser" should "be able to parse file" in {
    val filePath = getClass.getResource("../lobot/lobotSmall.lobot").getPath

    val lines = Utils.Control.using(io.Source.fromFile(filePath)(io.Codec.UTF8)) {
      source => (for (line <- source.getLines()) yield line).toList
    }
    val input = lines.mkString

    val parser = new ParserTop

    val x = for {
      tokens ← LobotLexer.parse(input).right
      ast ← parser(tokens).right
    } yield (tokens, ast)

    x match {
      case Left(value) => println(value)
      case Right(value) =>
        println("AST:" + value._2)
        println("Tokens:" + value._1)
    }
  }

}
