package ConfigParser


import scala.util.parsing.combinator.RegexParsers

abstract sealed class AnyArgument

case class IdRef(str: String) extends AnyArgument

case class FileDocRef(file: String, ref: String) extends AnyArgument

case class DOT()

case class ARROW()

trait IdentifierParser extends RegexParsers {
  def fileIdentifier: Parser[IdRef] = """[a-zA-Z]*[\w\-]*\w+""".r ^^ { str => IdRef(str) }

  def refIdentifier: Parser[IdRef] = """[a-zA-Z0-9]*[\w\s\[\-/\\&\]]*[a-zA-Z0-9\]]+""".r ^^ { str => IdRef(str) }
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
                          ){
  require(srcRef.file != trgRef.file, "Source and target file must be different")
}


final case class MasterModel(
                              name: String,
                              implicit_refinements: Map[String, List[RefinementModel]],
                              explicit_refinements: Map[String, List[RefinementModel]],
                            ){
  require(name.nonEmpty, "Master model name must not be empty")
  require(implicit_refinements.values.flatten.toSet.intersect(explicit_refinements.values.flatten.toSet).isEmpty, "Implicit and explicit refinements must be disjoint")
  require(implicit_refinements.forall(fileRefs => {
    fileRefs._2.forall(ref => {
      assert(ref.srcRef.file == fileRefs._1, "All implicit refinements must have the same source file. " + ref.srcRef.file + " != " + fileRefs._1)
      true
    })
  }), "All implicit refinements must have the same source file")
  require(explicit_refinements.forall(fileRefs => {
    fileRefs._2.forall(ref => {
      assert(ref.srcRef.file == fileRefs._1, "All explicit refinements must have the same source file. " + ref.srcRef.file + " != " + fileRefs._1)
      true
    })
  }), "All explicit refinements must have the same source file")
}

