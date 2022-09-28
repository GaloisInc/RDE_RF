package Parsers.LobotParser.Models

import scala.util.parsing.input.Positional

object AST {
  protected var id = 0
  protected def getNextId: Int  = { id = id + 1; id }
  def resetId():           Unit = id = 0
}

trait AST extends Positional {
  import AST._
  protected var id: Int = getNextId
  def getId: Int = id
}

trait Term extends AST
trait LiteralTerm extends Term
case class VariableTerm(name: String) extends Term
case class IntLiteralTerm(value: Int) extends LiteralTerm
case class BoolLiteralTerm(value: Boolean) extends LiteralTerm

trait Expression extends AST
case class SimpleExpression(lit: Option[LiteralTerm] = None, name: Option[VariableTerm] = None) extends Expression
case class BinOpExpression(op: String, term1: Expression, term2: Expression) extends Expression
case class UnOpExpression(op: String, term1: Expression) extends Expression
case class SelfExp() extends Expression
case class FieldAccessExp(exp: Expression, field: String) extends Expression
case class ApplicationExp(functionName: String, args: List[Expression]) extends Expression
case class SetExp(elements: Set[String]) extends Expression

case class FieldType(name: String, fieldType: LobotType) extends AST

trait LobotType extends AST
case class IntLobotType() extends LobotType
case class BoolLobotType() extends LobotType
case class EnumType(elements: Set[String]) extends LobotType
case class SetType(elementType: LobotType) extends LobotType
case class FunctionLobotType(argTypes: List[LobotType], retType: LobotType) extends LobotType
case class StructType(fields: List[FieldType]) extends LobotType
case class IdentifierTypes(fields: Set[String]) extends LobotType

trait Declaration extends AST

case class CheckDecl(name: String, fields: List[FieldType], whereClause: Option[Expression], thatClause: Option[Expression]) extends Declaration

case class KindDecl(name: String, lobotType: LobotType, restrictions: Option[Expression]) extends Declaration

case class TypeDecl(name: String, lobotType: LobotType) extends Declaration

case class AbstTypeDecl(name: String) extends Declaration

case class AbstFunctionDecl(name: String, lobotFunction: FunctionLobotType) extends Declaration

case class Specification(declarations: List[Declaration]) extends AST

