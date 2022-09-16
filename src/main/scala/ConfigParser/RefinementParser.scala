package ConfigParser


import scala.util.parsing.combinator.RegexParsers

abstract sealed class AnyArgument

case class IdRef(str: String) extends AnyArgument

case class FileDocRef(file: String, ref: String) extends AnyArgument

case class DOT()

case class ARROW()

trait IdentifierParser extends RegexParsers {
  def fileIdentifier: Parser[IdRef] = """[a-zA-Z][\w\-]*\w""".r ^^ { str => IdRef(str) }

  def refIdentifier: Parser[IdRef] = """[a-zA-Z0-9][\w\s\[\-/\\\&]*[\w\]]""".r ^^ { str => IdRef(str) }
}

class FileRefParser extends IdentifierParser {
  def dot: Parser[DOT] = "\\.".r ^^ { _ => DOT() }
  def file_doc_ref: Parser[FileDocRef] = fileIdentifier ~ dot ~ refIdentifier ^^ { case IdRef(fmu) ~ DOT() ~ IdRef(port) => FileDocRef(fmu, port) }
}


class RefinementParser extends FileRefParser {
  def arrow: Parser[ARROW] = "->".r ^^ { _ => ARROW() }

  def refinement: Parser[RefinementModel] = file_doc_ref ~ arrow ~ file_doc_ref ^^ {
    case FileDocRef(src_file, src_ref) ~ ARROW() ~ FileDocRef(trg_file, trg_ref) => RefinementModel(FileDocRef(src_file, src_ref), FileDocRef(trg_file, trg_ref))
  }
}

object RefinementParserSingleton extends RefinementParser

final case class RefinementModel(
                            srcRef: FileDocRef,
                            trgRef: FileDocRef,
                          )


final case class MasterModel(
                              name: String,
                              refinements: List[RefinementModel],
                            )