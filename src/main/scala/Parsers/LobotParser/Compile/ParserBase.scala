package Parsers.LobotParser.Compile

import Parsers.LobotParser.Models._
import org.apache.logging.log4j.scala.Logging

import scala.util.parsing.combinator.{PackratParsers, Parsers}
import scala.util.parsing.input.{NoPosition, Position, Reader}

class ParserBase extends Parsers with PackratParsers with Logging {

  protected val debugRules = true
  protected val debugMatch = true
  protected val tokenDebug = false

  //
  // TOKENS
  //
  override type Elem = Token

  class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
    if (tokenDebug) logger.trace(s"Tokens\n ${tokens.take(30)}")

    override def first: Token = tokens.head

    override def atEnd: Boolean = tokens.isEmpty

    override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)

    override def rest: Reader[Token] = new TokenReader(tokens.tail)
  }

  //
  // DEBUG
  //
  def dbg[T](p: => Parser[T])(name: String): Parser[T] = {
    if (!debugRules) p else
      name match {
        case "kindDecl" ⇒ log(p)(name)
        case "typeDecl" ⇒ log(p)(name)
        case "checkDecl" ⇒ log(p)(name)
        case "abstTypeDecl" ⇒ log(p)(name)
        case "abstFunctionDecl" ⇒ log(p)(name)
        case "wrappedExpression" ⇒ log(p)(name)
        case _ ⇒ p
      }
  }

  lazy val fieldTypeParser: PackratParser[FieldType] = positioned {
    variable ~ COLON() ~ typeParser ~ opt(NEWLINE()) ^^ {
      case v ~ _ ~ t ~ _ ⇒ FieldType(v.name, t)
    }
  }

  lazy val kindNames: PackratParser[FieldType] = positioned {
    variable ~ COLON() ~ typeParser ^^ {
      case v ~ _ ~ t ⇒ FieldType(v.name, t)
    }
  }


  lazy val typeParser: PackratParser[LobotType] = positioned {
    INTEGER() ^^^ IntLobotType() |
      BOOLEAN() ^^^ BoolLobotType() |
      IDENTIFIER("int") ^^^ IntLobotType() |
      OPEN_CURLY() ~> rep1sep(variable, COMMA()) <~ CLOSE_CURLY() ^^ (types => EnumType(types.map(_.name).toSet)) |
      SUBSET() ~ typeParser ^^ { case _ ~ t => SetType(t) } |
      STRUCT() ~ opt(NEWLINE()) ~ WITH() ~opt(NEWLINE())~ rep1sep(fieldTypeParser, COMMA()) ^^ {
        case _ ~ _ ~ _ ~ _ ~ fields ⇒ StructType(fields)
      } |
      rep1(identifier) ^^ { case idents => IdentifierTypes(idents.map(_.str).toSet) }
  }

  lazy val variable: PackratParser[VariableTerm] = positioned {
    identifier ^^ {
      case IDENTIFIER(x) ⇒ VariableTerm("%s".format(x))
    }
  }

  lazy val functionLobotParser: PackratParser[FunctionLobotType] = positioned {
    OPEN_PAREN() ~> repsep(typeParser, COMMA()) ~ CLOSE_PAREN() ~ ARROW() ~ typeParser ~ NEWLINE() ^^ {
      case args ~ _ ~ _ ~ ret ~ _   => FunctionLobotType(args, ret)
    }
  }

  lazy val operator1: PackratParser[BIN_OP1] = positioned {
    accept("operator1", {
      case id@BIN_OP1(name) =>
        if (debugMatch) logger.info(s"PARSE: operator1 $id")
        id
    })
  }

  lazy val operator2: PackratParser[BIN_OP2] = positioned {
    accept("operator2", {
      case id@BIN_OP2(name) =>
        if (debugMatch) logger.info(s"PARSE: operator2 $id")
        id
    })
  }

  lazy val operator3: PackratParser[BIN_OP3] = positioned {
    accept("operator3", {
      case id@BIN_OP3(name) =>
        if (debugMatch) logger.info(s"PARSE: operator3 $id")
        id
    })
  }

  lazy val operator3Eq: PackratParser[BIN_OP3] = positioned {
    accept("operator3Eq", {
      case id@EQUAL() =>
        if (debugMatch) logger.info(s"PARSE: operator3 $id")
        BIN_OP3("=")
    })
  }

  lazy val operator4: PackratParser[BIN_OP4] = positioned {
    accept("operator4", {
      case id@BIN_OP4(name) =>
        if (debugMatch) logger.info(s"PARSE: operator4 $id")
        id
    })
  }

  lazy val ambiguous_op: PackratParser[BIN_OP2] = positioned {
    MINUS() ^^ { _ ⇒
      BIN_OP2("-")
    }
  }

  lazy val identifier: PackratParser[IDENTIFIER] = positioned {
    accept("identifier", {
      case id@IDENTIFIER(name) =>
        if (debugMatch) logger.info(s"PARSE: identifier $id")
        id
    })
  }

  lazy val literal: PackratParser[LITERAL] = positioned {
    accept(
      "literal", {
        case lit@INT_LITERAL(name) ⇒
          if (debugMatch) logger.info(s"PARSE: literal int $lit")
          lit
        case lit@BOOL_LITERAL(name) ⇒
          if (debugMatch) logger.info(s"PARSE: literal bool  $lit")
          lit
      }
    )
  }


}
