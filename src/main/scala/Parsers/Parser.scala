package Parsers

import Parsers.ParserTypes.{ParsedDocument, ParsedRef}
import Types.{DocumentType, ReferenceType}

import scala.util.matching.Regex

trait Parser {
  def parse(fileToAnalyze: String): ParsedDocument
}

object CryptolParser {
  //Regex to match
  val typeRegex: Regex = """^type\s+(.*?)\s*=.*""".r
  val propertyRegex: Regex = """^property\s+(.*?)\s+.*""".r
  val eventRegex: Regex = """^(.*?)\s*:\s*.*\s*->.*""".r
  val importRegex: Regex = """^import\s+(\w*)\s*::\s*(\w*).*""".r


  def parseLine(line: String, documentName: String): Option[ParsedRef] = {
    cleanLine(line) match {
      case typeRegex(name) => Some(ParsedRef(name, line, documentName, DocumentType.Cryptol, ReferenceType.Type, ""))
      case propertyRegex(name) => Some(ParsedRef(name, line, documentName, DocumentType.Cryptol, ReferenceType.Requirement, ""))
      case eventRegex(name) => Some(ParsedRef(name, line, documentName, DocumentType.Cryptol, ReferenceType.Event, ""))
      case importRegex(name, _) => Some(ParsedRef(name, line, documentName, DocumentType.Cryptol, ReferenceType.Import, ""))
      case _ => None
    }
  }

  def cleanLine(line: String): String = {
    line.replaceAll("""\s+""", " ").trim
  }
}



