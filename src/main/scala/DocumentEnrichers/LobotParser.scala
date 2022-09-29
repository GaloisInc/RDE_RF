package DocumentEnrichers

import DocumentEnrichers.Parser._

import scala.util.parsing.combinator.RegexParsers


// Keywords
/*

sealed abstract class Keyword

case class Type() extends Keyword

case class With() extends Keyword

case class Where() extends Keyword

case class OF() extends Keyword

case class KIND() extends Keyword

case class CHECK() extends Keyword

case class ON() extends Keyword

case class THAT() extends Keyword


trait IdentifierParser extends RegexParsers {
  def identifier: Parser[IdRef] = """[\w_]+""".r ^^ { str => IdRef(str) }
}

trait DeclParser extends RegexParsers with IdentifierParser {
  def decl: Parser[Decl] = "type" ~> identifier ~ "with" ~ identifier ~ "where" ~ identifier ~ "of" ~ identifier ~ "kind" ~ identifier ~ "check" ~ identifier ~ "on" ~ identifier ~ "that" ~ identifier ^^ {
    case id1 ~ _ ~ id2 ~ _ ~ id3 ~ _ ~ id4 ~ _ ~ id5 ~ _ ~ id6 ~ _ ~ id7 ~ _ ~ id8 => Decl(id1, id2, id3, id4, id5, id6, id7, id8)
  }

  def kindDecl : Parser[KindDecl] = "type" ~> identifier ~ "with" ~ "of"  ~ kindType ~ "where" ~ identifier ~ "of" ~ identifier ~ "kind" ~ identifier ^^ {
    case id1 ~ _ ~ id2 ~ _ ~ id3 ~ _ ~ id4 ~ _ ~ id5 => KindDecl(id1, id2, id3, id4, id5)
  }


  def decls: Parser[List[Decl]] = rep(decl)
}


trait LobotLogicalOperatorParser extends RegexParsers with IdentifierParser {
  def and: Parser[AND] = "&" ^^ { _ => AND() }

  def or: Parser[OR] = "|" ^^ { _ => OR() }

  def not: Parser[NOT] = "!" ^^ { _ => NOT() }

  def imp: Parser[IMP] = "=>" ^^ { _ => IMP() }

  def iff: Parser[IFF] = "<=>" ^^ { _ => IFF() }

  def logicalOperator: Parser[LogicalOperator] = and | or | not | imp | iff

  def logicalExpression: Parser[Any] = identifier ~ logicalOperator ~ identifier ^^ { case id1 ~ op ~ id2 => (id1, op, id2) }

}

trait LobotTypeParser extends IdentifierParser {
  def booleanType: Parser[BOOLEAN] = "bool" ^^ { _ => BOOLEAN() }

  def integerType: Parser[INTEGER] = "int" ^^ { _ => INTEGER() }

  def subsetType: Parser[SUBSET] = "subset" ^^ { _ => SUBSET() }

  def kindNames : Parser[List[IdRef]] = rep(identifier)

  def lobotType: Parser[LobotType] = booleanType | integerType | identifier ^^ { id => id }


}

trait LobotKeywordParser extends RegexParsers {
  def rule: Parser[RULE] = "rule" ^^ { _ => RULE() }

  def typedef: Parser[TYPEDEF] = "type" ^^ { _ => TYPEDEF() }

  def stuctKeyword: Parser[STRUCT] = "struct" ^^ { _ => STRUCT() }

  def withKeyword: Parser[With] = "with" ^^ { _ => With() }

  def kindKeyWord: Parser[KIND] = "kind" ^^ { _ => KIND() }

  def whereKeyword: Parser[Where] = "where" ^^ { _ => Where() }

  def ofKeyWord: Parser[OF] = "of" ^^ { _ => OF() }
}

class LobotParser extends IdentifierParser
  with LobotTypeParser
  with SymbolParser
  with ComparisonOperatorParser
  with LobotKeywordParser {

  def identifierList: Parser[List[IdRef]] = rep1sep(identifier, comma)

  def field = identifier ~ colon ~ typeSpec ^^ { case id ~ _ ~ t => LobotField(id, t) }

  def fieldList : Parser[List[IdRef]] = rep1sep(field, newline)

  def TypeDefParser: Parser[TypeDef] = typedef ~ identifier ~ eq ~ lbrace ~ identifierList ~ rbrace ^^ {
    case _ ~ IdRef(name) ~ _ ~ _ ~ fields ~ _ => TypeDef(name, fields.map {
      case IdRef(name) => name
    })
  }

  def boolExpression: Parser[BoolExpression] = (identifier | Lobotvalue) ~ comparisonOperator ~ identifier ^^ {
    case IdRef(id1) ~ op ~ IdRef(id2) => BoolExpression(id1, op, id2)
  }

  def KindParser: Parser[Kind] = identifier ~ colon ~ kindKeyWord ~ ofKeyWord ~ booleanType  ~
    whereKeyword ~ boolExpression ^^ {
    case IdRef(name) ~ _ ~ _ ~ _ ~ TypeDef(typeName, _) ~ _ ~ boolExpression => Kind(name, typeName, boolExpression)
  }

  def StructParser: Parser[LobotStruct] = identifier ~ colon ~ kindKeyWord ~ ofKeyWord ~ stuctKeyword ~ withKeyword ~
    fieldList ^^ {
    case IdRef(name) ~ _ ~ _ ~ _ ~ TypeDef(typeName, _) ~ _ ~ boolExpression => Struct(name, typeName, boolExpression)
  }


}

object LobotParserSingleton extends LobotParser

case class TypeDef(name: String, fields: List[String])

case class Kind(name: String, name: String, boolExpression: BoolExpression)

case class BoolExpression(id1: String, op: ComparisonOperator, id2: String)

case class LobotStruct(name: String, fields: List[String], restrictions: List[BoolExpression])

case class LobotField(name: String, lobotType: LobotType)
*/


