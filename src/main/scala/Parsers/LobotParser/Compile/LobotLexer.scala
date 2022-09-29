package Parsers.LobotParser.Compile


import Parsers.LobotParser.Models._

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

//
// LEXER
//
object LobotLexer extends RegexParsers {
  override def skipWhitespace: Boolean = true
  override val whiteSpace: Regex = "[ \t\r\f]+".r


  def parse(code: String): Either[LexerError, List[Token]] = {
    parse(tokens, code) match {
      case NoSuccess(msg,  next) ⇒ Left(LexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, _)    ⇒ Right(result)
      case _                     ⇒ Left(LexerError(Location(0, 0), "Unknown error"))
    }
  }

  def tokens: Parser[List[Token]] = {
    phrase(
      rep1(withToken | ofToken  | onToken | structToken | kindToken | whereToken | checkToken | absToken | selfToken
      | colon | comma | semi | integer  | booleanToken | operator1 | operator2 | operator3 | operator4 | equal | typeToken | newLine
      | oParen | cParen | oCurly | cCurly | dot | arrow | subset | abstractToken | that | literal | identifier
      )) ^^ { rawTokens => rawTokens.filter(_ != COMMENT()) }
  }

  def identifier: Parser[IDENTIFIER] = positioned { "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => IDENTIFIER(str) } }
  def literal   : Parser[LITERAL]    = positioned { """[0-9]+""".r             ^^ { dub ⇒ INT_LITERAL(dub.toInt) } |
    ("true" | "false") ^^ {dub => BOOL_LITERAL(dub.toBoolean) }}
  def operator1: Parser[BIN_OP1]     = positioned { ("*" | "%" | "/" | "+")           ^^ { x ⇒ BIN_OP1(x) } }
  def operator2: Parser[BIN_OP2]     = positioned { ("in" | "notin"| "subset" | "size")         ^^ { x ⇒ BIN_OP2(x) } }
  def operator3: Parser[BIN_OP3]     = positioned { (">=" | "<=" | ">" | "<"| "==")  ^^ { x ⇒ BIN_OP3(x) } }
  def operator4: Parser[BIN_OP4]     = positioned { ("&" | "|" | "^" | "=>" | "<=>")      ^^ { x ⇒ BIN_OP4(x) } }
  def integer:   Parser[INTEGER]       = positioned { "int"                    ^^ (_ ⇒ INTEGER()) }
  def booleanToken:   Parser[BOOLEAN]  = positioned { "bool"              ^^ (_ ⇒ BOOLEAN()) }
  def oCurly:    Parser[OPEN_CURLY]    = positioned { "{"                 ^^ (_ ⇒ OPEN_CURLY()) }
  def cCurly:    Parser[CLOSE_CURLY]   = positioned { "}"                 ^^ (_ ⇒ CLOSE_CURLY()) }
  def oParen:    Parser[OPEN_PAREN]    = positioned { "("                 ^^ (_ ⇒ OPEN_PAREN()) }
  def cParen:    Parser[CLOSE_PAREN]   = positioned { ")"                 ^^ (_ ⇒ CLOSE_PAREN()) }
  def equal:     Parser[EQUAL]         = positioned { "="                 ^^ (_ ⇒ EQUAL()) }
  def colon:     Parser[COLON]         = positioned { ":"                 ^^ (_ ⇒ COLON()) }
  def semi:      Parser[SEMI]          = positioned { ";"                 ^^ (_ ⇒ SEMI()) }
  def dot:      Parser[DOT]            = positioned { "."                 ^^ (_ ⇒ DOT()) }
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
  def typeToken:      Parser[TYPE]       = positioned { "type"               ^^ (_ ⇒ TYPE()) }
  def subset:     Parser[SUBSET]       = positioned { "subset"               ^^ (_ ⇒ SUBSET()) }
  def arrow:     Parser[ARROW]       = positioned { "->"               ^^ (_ ⇒ ARROW()) }
  def abstractToken:     Parser[ABSTRACT]       = positioned { "abstract"               ^^ (_ ⇒ ABSTRACT()) }
  def that:     Parser[THAT]       = positioned { "that"               ^^ (_ ⇒ THAT()) }
  def newLine : Parser[NEWLINE]        = positioned { "\n"                ^^ (_ ⇒ NEWLINE()) }


  def singleLineComment: Parser[COMMENT] = "--" ~ rep(not("\n") ~ ".".r) ^^^ COMMENT()
  def multiLineComment:  Parser[COMMENT] = "/*" ~ rep(not("*/") ~ "(?s).".r) ~ "*/" ^^^ COMMENT()
}
