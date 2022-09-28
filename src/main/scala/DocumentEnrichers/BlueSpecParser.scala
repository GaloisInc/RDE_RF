package DocumentEnrichers


import DocumentEnrichers.Parser._

import scala.util.parsing.combinator.RegexParsers


trait TypeParser extends RegexParsers {
  def boolean: Parser[BOOLEAN] = "Bool" ^^ { _ => BOOLEAN() }

  def bit: Parser[BIT] = "Bit" ^^ { _ => BIT() }

  def vector: Parser[VECTOR] = "Vector" ^^ { _ => VECTOR() }

  def action: Parser[ACTION] = "Action" ^^ { _ => ACTION() }

  def actionValue: Parser[ACTIONVALUE] = "ActionValue" ^^ { _ => ACTIONVALUE() }

  def unitType = (boolean | action)

  def composedType = vector | actionValue | bit

}

trait KeywordParser extends RegexParsers {
  def rule: Parser[RULE] = "rule" ^^ { _ => RULE() }

  def module: Parser[MODULE] = "module" ^^ { _ => MODULE() }

  def importKeyword: Parser[IMPORT] = "import" ^^ { _ => IMPORT() }

  def interface: Parser[INTERFACE] = "interface" ^^ { _ => INTERFACE() }

  def method: Parser[METHOD] = "method" ^^ { _ => METHOD() }

  def typedef: Parser[TYPEDEF] = "typedef" ^^ { _ => TYPEDEF() }

  def stuctKeyword: Parser[STRUCT] = "struct" ^^ { _ => STRUCT() }

  def enumKeyword: Parser[ENUM] = "enum" ^^ { _ => ENUM() }

  def packageKeyword: Parser[PACKAGE] = "package" ^^ { _ => PACKAGE() }

  def function: Parser[FUNCTION] = "function" ^^ { _ => FUNCTION() }

  def direction: Parser[DIRECTION] = ("in".r | "out".r) ^^ { str => DIRECTION() }

  def derivingKeyword: Parser[DERIVING] = "deriving" ^^ { _ => DERIVING() }

}

class BlueSpecParser extends IdentifierParser
  with TypeParser
  with SymbolParser
  with KeywordParser {

  /*
  def methodParsing: Parser[BlueSpecMethod] = method ~ opt(typeParser) ~ identifier ~ opt(lparen ~ rep(argumentParser) ~ rparen) ^^ {
    case _ ~ _ ~ IdRef(methodName) ~ Some(_ ~ arguments ~ _) => BlueSpecMethod(methodName, None, arguments)
    case _ ~ None ~ IdRef(methodName) ~ Some(_ ~ arguments ~ _) => BlueSpecMethod(methodName, None, arguments)
    case _ ~ _ ~ IdRef(methodName) ~ None => BlueSpecMethod(methodName, None, List())
    case _ ~ None ~ IdRef(methodName) ~ None => BlueSpecMethod(methodName, None, List())
  }

   */

  def packageParsing: Parser[BlueSpecPackage] = packageKeyword ~ identifier ~ semicolon ^^ {
    case _ ~ IdRef(packageName) ~ _ => BlueSpecPackage(packageName)
  }


  /*
    def typeParser: Parser[BlueSpecType] = unitType ^^ {
      case BOOLEAN() => BlueSpecBoolType()
      case ACTION() => BlueSpecActionType()
    } |
      (composedType ~ hashtag ~ lparen ~ opt(typeParser) ~ numberIdentifier ~ rparen) ^^ {
        case bit ~ _ ~ _ ~ None ~ NumberIdRef(size) ~ _ => BlueSpecBitArrayType(size)
        case vector ~ _ ~ Some(innerType) ~ NumberIdRef(size) ~ _ => BlueSpecVectorType(innerType, size)
        case actionValue ~ _ ~ _ ~ NumberIdRef(size) ~ _ => BlueSpecActionValueType(size)
      }

   */

  def typeStructParsing: Parser[BlueSpecStructType] = typedef ~ stuctKeyword ~ lbrace ~ rep(argumentParser) ~ rbrace ~ identifier ~ semicolon ^^ {
    case _ ~ _ ~ _ ~ _ ~ _ ~ IdRef(typeName) ~ _ => BlueSpecStructType(typeName)
  }

  def derivingParsing: Parser[String] = derivingKeyword ~ lparen ~ identifier ~ rep(comma ~ identifier) ~ rparen ^^ {
    case _ ~ _ ~ IdRef(derivingName) ~ _ ~ _ => derivingName
  }

  def identifierList: Parser[List[IdRef]] = rep1sep(identifier, comma)

  def typeEnumsParsing: Parser[BlueSpecEnumType] = typedef ~ enumKeyword ~ lbrace ~ identifierList ~ rbrace ~ identifier ~ opt(derivingParsing) ~ semicolon ^^ {
    case _ ~ _ ~ _ ~ enumList ~ _ ~ IdRef(typeName) ~ derivingName ~ _ => BlueSpecEnumType(typeName)
  }

  def moduleParsing: Parser[BlueSpecModule] = module ~ identifier ~ lparen ~ identifier ~ rparen ~ semicolon ^^ {
    case _ ~ IdRef(moduleName) ~ _ ~ IdRef(argument) ~ _ ~ _ => BlueSpecModule(moduleName, argument)
  }

  def interfaceParsing: Parser[BlueSpecInterface] = interface ~ identifier ~ opt(identifier) ~ semicolon ^^ {
    case _ ~ IdRef(interfaceName) ~ name ~ _ =>
      name match {
        case Some(IdRef(name)) => BlueSpecInterface(interfaceName, Some(name))
        case None => BlueSpecInterface(interfaceName)
      }
  }

  def ruleParsing: Parser[BlueSpecRule] = rule ~ identifier ~ semicolon ^^ {
    case _ ~ IdRef(interfaceName) ~ _ => BlueSpecRule(interfaceName)
  }

  def blueSpecImportParsing: Parser[BlueSpecImport] = importKeyword ~ identifier ~ repN(2, colon) ~ astrix ~ semicolon ^^ {
    case _ ~ IdRef(importName) ~ _ ~ _ ~ _ => BlueSpecImport(importName)
  }

  def uniType: Parser[Product] = boolean | action

  def composableTypes: Parser[Product] = vector | actionValue | bit

  def typeParser: Parser[Product] = uniType | composableTypes

  def argumentParser: Parser[String] = typeParser ~ identifier ^^ {
    case _ ~ IdRef(argumentName) => argumentName
  }
}

object BlueSpecParserSingleton extends BlueSpecParser

final case class BlueSpecImport(importName: String) {
  require(importName.nonEmpty, "importName must not be empty")
}

final case class BlueSpecMethod(name: String, returnType: Option[String], parameters: List[String]) {
  require(name.nonEmpty, "Method name cannot be empty")
}


final case class BlueSpecModule(name: String, argument: String) {
  require(name.nonEmpty, "Module name cannot be empty")
  require(argument.nonEmpty, "Module argument cannot be empty")
}

final case class BlueSpecInterface(name: String, argument: Option[String] = None) {
  require(name.nonEmpty, "Interface name cannot be empty")
}

final case class BlueSpecRule(name: String) {
  require(name.nonEmpty, "Rule name cannot be empty")
}

final case class BlueSpecPackage(name: String) {
  require(name.nonEmpty, "Package name cannot be empty")
}

final case class BlueSpecEnumType(name: String) {
  require(name.nonEmpty, "Type name cannot be empty")
}

final case class BlueSpecStructType(name: String) {
  require(name.nonEmpty, "Type name cannot be empty")
}

trait BlueSpecType {}

final case class BlueSpecBoolType() extends BlueSpecType

final case class BlueSpecActionType() extends BlueSpecType

final case class BlueSpecVectorType(subType: BlueSpecType, size: Int) extends BlueSpecType {
  require(size > 0, "Vector size must be greater than 0")
}

final case class BlueSpecBitArrayType(size: Int) extends BlueSpecType {
  require(size > 0, "Bit size must be greater than 0")
}

final case class BlueSpecActionValueType(size: Int) extends BlueSpecType {
  require(size > 0, "ActionValue size must be greater than 0")
}




