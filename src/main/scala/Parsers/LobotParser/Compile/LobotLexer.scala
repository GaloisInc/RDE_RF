package Parsers.LobotParser.Compile


import Parsers.LobotParser.Models.parser_combinator.model._
import Parsers.LobotParser.Models.{LexerError, Location}

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

//
// LEXER
//
object LobotLexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace: Regex = "[\t\r\f\n]+".r


  def parse(code: String): Either[LexerError, List[Token]] = {
    parse(tokens, code) match {
      case NoSuccess(msg,  next) ⇒ Left(LexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, _)    ⇒ Right(result)
    }
  }

  def tokens: Parser[List[Token]] = {
    phrase(
      rep1(withToken | ofToken | kindToken | onToken | structToken | kindToken
      | equal | colon | comma | semi | integer | whereToken | checkToken | absToken | selfToken
      | oParen | cParen | oCurly | cCurly | booleanToken | operator2 | operator3 | operator4
      | literal | identifier
    )) ^^ { rawTokens => rawTokens.filter(_ != COMMENT()) }
  }

  def identifier: Parser[IDENTIFIER] = positioned { "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => IDENTIFIER(str) } }
  def literal   : Parser[LITERAL]    = positioned { """[0-9]+""".r             ^^ { dub ⇒ INT_LITERAL(dub.toInt) } |
    ("true" | "false") ^^ {dub => BOOL_LITERAL(dub.toBoolean) }}

  //def operator1: Parser[BIN_OP1]     = positioned { """([*%/])""".r           ^^ { x ⇒ BIN_OP1(x) } }
  def operator2: Parser[BIN_OP2]     = positioned { """(\+|>>|<<)""".r         ^^ { x ⇒ BIN_OP2(x) } }
  def operator3: Parser[BIN_OP3]     = positioned { """(>=|<=|<|>|!=|==)""".r  ^^ { x ⇒ BIN_OP3(x) } }
  def operator4: Parser[BIN_OP4]     = positioned { """(\||&|^)""".r           ^^ { x ⇒ BIN_OP4(x) } }
  def integer:   Parser[INTEGER]       = positioned { "int"                    ^^ (_ ⇒ INTEGER()) }
  def booleanToken:   Parser[BOOLEAN]  = positioned { "bool"              ^^ (_ ⇒ BOOLEAN()) }
  def oCurly:    Parser[OPEN_CURLY]    = positioned { "{"                 ^^ (_ ⇒ OPEN_CURLY()) }
  def cCurly:    Parser[CLOSE_CURLY]   = positioned { "}"                 ^^ (_ ⇒ CLOSE_CURLY()) }
  def oParen:    Parser[OPEN_PAREN]    = positioned { "("                 ^^ (_ ⇒ OPEN_PAREN()) }
  def cParen:    Parser[CLOSE_PAREN]   = positioned { ")"                 ^^ (_ ⇒ CLOSE_PAREN()) }
  def equal:     Parser[EQUAL]         = positioned { "="                 ^^ (_ ⇒ EQUAL()) }
  def colon:     Parser[COLON]         = positioned { ":"                 ^^ (_ ⇒ COLON()) }
  def semi:      Parser[SEMI]          = positioned { ";"                 ^^ (_ ⇒ SEMI()) }
  def comma:     Parser[COMMA]         = positioned { ","                 ^^ (_ ⇒ COMMA()) }
  def minus:     Parser[MINUS]         = positioned { "-"                 ^^ (_ ⇒ MINUS()) }
  def withToken:     Parser[WITH]      = positioned { "with"              ^^ (_ ⇒ WITH()) }
  def ofToken:     Parser[OF]          = positioned { "of"                ^^ (_ ⇒ OF()) }
  def kindToken:     Parser[KIND]      = positioned { "kind"              ^^ (_ ⇒ KIND()) }
  def onToken:     Parser[ON]          = positioned { "on"                ^^ (_ ⇒ ON()) }
  def structToken:     Parser[STRUCT]  = positioned { "struct"            ^^ (_ ⇒ STRUCT()) }
  def whereToken:     Parser[WHERE]    = positioned { "where"             ^^ (_ ⇒ WHERE()) }
  def checkToken:     Parser[CHECK]    = positioned { "check"             ^^ (_ ⇒ CHECK()) }
  def selfToken:      Parser[SELF]     = positioned { "self"              ^^ (_ ⇒ SELF()) }
  def absToken:      Parser[ABS]       = positioned { "abs"               ^^ (_ ⇒ ABS()) }

  def singleLineComment: Parser[COMMENT] = "--" ~ rep(not("\n") ~ ".".r) ^^^ COMMENT()
  def multiLineComment:  Parser[COMMENT] = "/*" ~ rep(not("*/") ~ "(?s).".r) ~ "*/" ^^^ COMMENT()
}
