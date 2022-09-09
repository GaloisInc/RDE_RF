package Parsers

import Parsers.ParserTypes.{ParsedDocument, ParsedRef, ParsedReference, Relation}
import Types.{DocumentType, ReferenceType}
import Utils.FileUtil

import scala.util.matching.Regex

object LandoParser extends Parser {
  //Regex to match
  val relationRegex: Regex = """^relation\s*(?:(.*?)\s+(contains|client|inherit))\s+(.*)""".r
  //All components, systems and subsystems are referenced by their name which start with a capital letter
  val componentRegex: Regex = """^component\s+([A-Z].*?)(?:\s+\((.*)\))?""".r
  val systemRegex: Regex = """^system\s+([A-Z].*?)(?:\s+(?:\((.*)\)))?""".r
  val subsystemRegex: Regex = """^subsystem\s+([A-Z].*?)(?:\s+\((.*)\))?""".r
  val lineCommentsRegex: Regex = """^//.*""".r
  val multiLineCommentsRegex: Regex = """^/\*.*""".r
  val emptyLineRegex: Regex = """^\s*$""".r

  def parse(fileToAnalyze: String): ParsedDocument = {
    require(fileToAnalyze.nonEmpty, "File to analyze cannot be empty")

    val lines = Utils.Control.using(io.Source.fromFile(fileToAnalyze)) {
      source => (for (line <- source.getLines()) yield line).toList
    }

    val parsedLines = lines.map(parseLine).filter(_.isDefined).map(_.get)
    val documentName = FileUtil.getFileName(fileToAnalyze)

    ParsedDocument(
      documentName,
      fileToAnalyze,
      parsedLines.toSet,
      DocumentType.Lando
    )
  }

  def parseLine(line: String): Option[ParsedReference] = {
    cleanLine(line) match {
      case lineCommentsRegex() | multiLineCommentsRegex() | emptyLineRegex() => None
      case relationRegex(source, relationType, target) => Some(Relation("", line, ReferenceType.Relation, source, relationType, target))
      case componentRegex(name, acronym) => Some(ParsedRef(name, line, ReferenceType.Component, acronym))
      case systemRegex(name, acronym) => Some(ParsedRef(name, line, ReferenceType.System, acronym))
      case subsystemRegex(name, acronym) => Some(ParsedRef(name, line, ReferenceType.SubSystem, acronym))
      case _ => None
    }
  }

  def cleanLine(line: String): String = {
    line.strip().stripSuffix("{").stripSuffix("}").stripSuffix(":;").stripSuffix(";")
  }
}
