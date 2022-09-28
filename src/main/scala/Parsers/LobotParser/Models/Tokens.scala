package Parsers.LobotParser.Models

package parser_combinator.model

import scala.util.parsing.input.Positional

//
// TOKEN DEFINITIONS
//
sealed trait Token extends Positional
sealed trait LITERAL extends Token
case class INT_LITERAL(int: Int) extends LITERAL
case class BOOL_LITERAL(int: Boolean) extends LITERAL
case class IDENTIFIER(str: String) extends Token
case class BIN_OP1(str: String) extends Token
case class BIN_OP2(str: String) extends Token
case class BIN_OP3(str: String) extends Token
case class BIN_OP4(str: String) extends Token
case class MINUS() extends Token
case class INTEGER() extends Token
case class BOOLEAN() extends Token
case class OPEN_CURLY() extends Token
case class CLOSE_CURLY() extends Token
case class OPEN_PAREN() extends Token
case class CLOSE_PAREN() extends Token
case class EQUAL() extends Token
case class COMMENT() extends Token
case class COLON() extends Token
case class SEMI() extends Token
case class COMMA() extends Token
case class WITH() extends Token
case class OF() extends Token
case class KIND() extends Token
case class ON() extends Token
case class STRUCT() extends Token
case class WHERE() extends Token
case class CHECK() extends Token
case class ABS() extends Token
case class SELF() extends Token
