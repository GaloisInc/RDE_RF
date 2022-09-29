package DocumentEnrichers.Parser

import scala.util.parsing.combinator.RegexParsers


abstract sealed class AnyArgument

case class IdRef(str: String) extends AnyArgument

case class NumberIdRef(int: Int) extends AnyArgument

// Symbols
sealed abstract class Symbol

case class DOT() extends Symbol

case class COMMA() extends Symbol

case class LPAREN() extends Symbol

case class RPAREN() extends Symbol

case class LBRACKET() extends Symbol

case class RBRACKET() extends Symbol

case class COLON() extends Symbol

case class ASTRIX() extends Symbol

case class LBRACE() extends Symbol

case class RBRACE() extends Symbol

case class SEMICOLON() extends Symbol

case class HASHTAG() extends Symbol

// Keywords

case class RULE()

case class MODULE()

case class IMPORT()

case class INTERFACE()

case class METHOD()

case class TYPEDEF()

case class PACKAGE()

case class FUNCTION()

case class DIRECTION()

case class STRUCT()

case class ENUM()

case class DERIVING()


// Types
sealed abstract class Type

case class BOOLEAN() extends Type

case class BIT() extends Type

case class VECTOR() extends Type

case class ACTION() extends Type

case class ACTIONVALUE() extends Type

case class INTEGER() extends Type


trait IdentifierParser extends RegexParsers {
  def identifier: Parser[IdRef] = """\w+""".r ^^ { str => IdRef(str) }

  def numberIdentifier: Parser[NumberIdRef] = """\d+""".r ^^ { int => NumberIdRef(int.toInt) }
}


trait SymbolParser extends RegexParsers {
  def dot: Parser[DOT] = "." ^^ { _ => DOT() }

  def comma: Parser[COMMA] = "," ^^ { _ => COMMA() }

  def lparen: Parser[LPAREN] = "(" ^^ { _ => LPAREN() }

  def rparen: Parser[RPAREN] = ")" ^^ { _ => RPAREN() }

  def lbracket: Parser[LBRACKET] = "[" ^^ { _ => LBRACKET() }

  def rbracket: Parser[RBRACKET] = "]" ^^ { _ => RBRACKET() }

  def colon: Parser[COLON] = ":" ^^ { _ => COLON() }

  def astrix: Parser[ASTRIX] = "*" ^^ { _ => ASTRIX() }

  def lbrace: Parser[LBRACE] = "{" ^^ { _ => LBRACE() }

  def rbrace: Parser[RBRACE] = "}" ^^ { _ => RBRACE() }

  def semicolon: Parser[SEMICOLON] = ";" ^^ { _ => SEMICOLON() }

  def hashtag: Parser[HASHTAG] = "#" ^^ { _ => HASHTAG() }
}


// Comparison operators

sealed abstract class ComparisonOperator

case class EQ() extends ComparisonOperator

case class NEQ() extends ComparisonOperator

case class GT() extends ComparisonOperator

case class LT() extends ComparisonOperator

case class GTE() extends ComparisonOperator

case class LTE() extends ComparisonOperator

trait ComparisonOperatorParser extends RegexParsers {
  def eq: Parser[EQ] = "==" ^^ { _ => EQ() }

  def neq: Parser[NEQ] = "!=" ^^ { _ => NEQ() }

  def gt: Parser[GT] = ">" ^^ { _ => GT() }

  def lt: Parser[LT] = "<" ^^ { _ => LT() }

  def gte: Parser[GTE] = ">=" ^^ { _ => GTE() }

  def lte: Parser[LTE] = "<=" ^^ { _ => LTE() }

  def comparisonOperator: Parser[ComparisonOperator] = eq | neq | gt | lt | gte | lte

}

// Logical operators

sealed abstract class LogicalOperator

case class AND() extends LogicalOperator

case class OR() extends LogicalOperator

case class NOT() extends LogicalOperator

case class IMP() extends LogicalOperator

case class IFF() extends LogicalOperator