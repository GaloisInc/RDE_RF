package Parsers.LobotParser.Compile

import Parsers.LobotParser.Models._

class ParserTop extends ParserExpression {

  def apply(tokens: Seq[Token]): Either[ParserError, AST] = {
    val reader = new PackratReader(new TokenReader(tokens))
    lobotSpecParser(reader) match {
      case NoSuccess(msg, next) ⇒ Left(ParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, _) ⇒ Right(result)
    }
  }

  def lobotSpecParser: Parser[AST] = {
    rep1sep(declarationParser, rep(NEWLINE())) <~ opt(rep(NEWLINE())) ^^ { case decls => Specification(decls) }
  }

  lazy val declarationParser: PackratParser[Declaration] = positioned {
    val whereDecl = {
      opt(NEWLINE()) ~ WHERE() ~ opt(NEWLINE()) ~> expression4 ^^ {
        case e => e
      }
    }

    val thatDecl = {
      opt(NEWLINE()) ~ THAT() ~ opt(NEWLINE()) ~> expression4 ^^ {
        case e ⇒ e
      }
    }

    val kindDecl = {
      identifier ~ COLON() ~ KIND() ~ OF() ~ typeParser ~ opt(whereDecl) ^^ {
        case IDENTIFIER(str) ~ _ ~ _ ~ _ ~ t ~ w ⇒ KindDecl(str, t, w)
      }
    }

    val checkDecl = {
      identifier ~ COLON() ~ CHECK() ~ NEWLINE() ~ ON() ~ rep(fieldTypeParser) ~ opt(whereDecl) ~ opt(thatDecl) ^^ {
        case IDENTIFIER(str) ~ _ ~ _ ~ _ ~ _ ~ fields ~ w ~ t ⇒ CheckDecl(str, fields, w, t)
      }
    }

    val typeDecl = {
      TYPE() ~ identifier ~ EQUAL() ~ opt(NEWLINE()) ~ typeParser ^^ {
        case _ ~ IDENTIFIER(str) ~ _ ~ _ ~ t ⇒ TypeDecl(str, t)
      }
    }

    val abstTypeDecl = {
      ABSTRACT() ~ TYPE() ~ identifier ^^ {
        case _ ~ _ ~ IDENTIFIER(str) ⇒ AbstTypeDecl(str)
      }
    }

    val abstFunctionDecl = {
      ABSTRACT() ~ identifier ~ COLON() ~ functionLobotParser ^^ {
        case _ ~ IDENTIFIER(str) ~ _ ~ lobotFunction ⇒ AbstFunctionDecl(str, lobotFunction)
      }
    }

    dbg(kindDecl)(name = "kindDecl") |
      dbg(checkDecl)(name = "checkDecl") |
      dbg(typeDecl)(name = "typeDecl") |
      dbg(abstTypeDecl)(name = "abstTypeDecl") |
      dbg(abstFunctionDecl)(name = "abstFunctionDecl")
  }
}