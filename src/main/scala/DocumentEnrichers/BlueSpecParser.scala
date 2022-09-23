package DocumentEnrichers

import scala.util.parsing.combinator.RegexParsers

abstract sealed class AnyArgument

case class IdRef(str: String) extends AnyArgument

case class DOT()

case class LPAREN()

case class RPAREN()

case class LBRACKET()

case class RBRACKET()

case class COLON()

case class ASTRIX()

case class LBRACE()

case class RBRACE()

case class SEMICOLON()

case class RULE()

case class MODULE()

case class IMPORT()

case class INTERFACE()

case class METHOD()

case class TYPEDEF()

case class PACKAGE()

case class FUNCTION()

case class DIRECTION()



trait IdentifierParser extends RegexParsers {
  def identifier: Parser[IdRef] = """\W+""".r ^^ { str => IdRef(str) }
}

class BlueSpecParser extends IdentifierParser {
  def dot: Parser[DOT] = "\\.".r ^^ { _ => DOT() }

  def lparen: Parser[LPAREN] = "\\(".r ^^ { _ => LPAREN() }

  def rparen: Parser[RPAREN] = "\\)".r ^^ { _ => RPAREN() }

  def lbrace: Parser[LBRACE] = "\\{".r ^^ { _ => LBRACE() }

  def rbrace: Parser[RBRACE] = "\\}".r ^^ { _ => RBRACE() }

  def color: Parser[COLON] = ":".r ^^ { _ => COLON() }

  def astrix: Parser[ASTRIX] = "\\*".r ^^ { _ => ASTRIX() }

  def lbracket: Parser[LBRACKET] = "\\[".r ^^ { _ => LBRACKET() }

  def rbracket: Parser[RBRACKET] = "\\]".r ^^ { _ => RBRACKET() }

  def semicolon: Parser[SEMICOLON] = ";".r ^^ { _ => SEMICOLON() }

  def rule: Parser[RULE] = "rule".r ^^ { _ => RULE() }

  def module: Parser[MODULE] = "module".r ^^ { _ => MODULE() }

  def blueSpecImport: Parser[IMPORT] = "import".r ^^ { _ => IMPORT() }

  def interface: Parser[INTERFACE] = "interface".r ^^ { _ => INTERFACE() }

  def method: Parser[METHOD] = "method".r ^^ { _ => METHOD() }

  def blueSpecTypeDef: Parser[TYPEDEF] = "typedef".r ^^ { _ => TYPEDEF() }

  def blueSpecPackage: Parser[PACKAGE] = "package".r ^^ { _ => PACKAGE() }

  def direction: Parser[DIRECTION] = ("in".r | "out".r) ^^ { str => DIRECTION() }

  def function: Parser[FUNCTION] = "function".r ^^ { _ => FUNCTION() }

  def methodParsing: Parser[BlueSpecMethod] = method ~ direction ~ identifier ^^ {
    case _ ~ _ ~ IdRef(methodName) => BlueSpecMethod(methodName)
  }

  def packageParsing: Parser[BlueSpecPackage] = blueSpecPackage ~ identifier ~ semicolon ^^ {
    case _ ~ IdRef(packageName) => BlueSpecPackage(packageName)
  }

  def typeParsing: Parser[BlueSpecType] = blueSpecTypeDef ~ lbrace ~ identifier ~ rbrace ~ identifier ~ semicolon ^^ {
    case _ ~ _ ~ _ ~ _ ~ IdRef(typeName) => BlueSpecType(typeName)
  }

  def moduleParsing: Parser[BlueSpecModule] = module ~ identifier ~ lparen ~ identifier ~ rparen ~ semicolon ^^ {
    case _ ~ IdRef(moduleName) ~ _ ~ IdRef(argument) ~ _ ~ _ => BlueSpecModule(moduleName, argument)
  }

  def interfaceParsing: Parser[BlueSpecInterface] = interface ~ identifier ~ COLON ~ COLON ~ ASTERIX ~ semicolon ^^ {
    case _ ~ IdRef(interfaceName) ~ _ ~ _ ~ _ ~ _ => BlueSpecInterface(interfaceName)
  }

  def ruleParsing: Parser[BlueSpecRule] = rule ~ identifier ~ semicolon ^^ {
    case _ ~ IdRef(interfaceName) ~ _ => BlueSpecRule(interfaceName)
  }

  def blueSpecImportParsing: Parser[BlueSpecImport] = blueSpecImport ~ identifier ~ semicolon ^^ {
    case _ ~ IdRef(importName) ~ _ => BlueSpecImport(importName)
  }

}

final case class BlueSpecImport(importName: String) {
  require(importName.nonEmpty, "importName must not be empty")
}

final case class BlueSpecMethod(name: String) {
  require(name.nonEmpty, "Method name cannot be empty")
}


final case class BlueSpecModule(name: String, argument: String) {
  require(name.nonEmpty, "Module name cannot be empty")
  require(argument.nonEmpty, "Module argument cannot be empty")
}

final case class BlueSpecInterface(name: String) {
  require(name.nonEmpty, "Interface name cannot be empty")
}

final case class BlueSpecRule(name: String) {
  require(name.nonEmpty, "Rule name cannot be empty")
}

final case class BlueSpecPackage(name: String) {
  require(name.nonEmpty, "Package name cannot be empty")
}

final case class BlueSpecType(name: String) {
  require(name.nonEmpty, "Type name cannot be empty")
}

