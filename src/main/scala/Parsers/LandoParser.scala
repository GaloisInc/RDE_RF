package Parsers

import Parsers.ParserTypes.{ParsedDocument, ParsedRef, ParsedReference, Relation}
import Types.{DocumentType, ReferenceType}
import Utils.FileUtil

import scala.io.{Codec, Source}
import scala.util.matching.Regex

object LandoParser extends Parser {
  //Regex to match
  val relationRegex: Regex = """^relation\s*(.*?)\s+(contains|client|inherit)\s+(.*)""".r
  //All components, systems and subsystems are referenced by their name which start with a capital letter
  val componentRegex: Regex = """^component\s+([A-Z].*?)(?:\s+\((.*)\))?""".r
  val systemRegex: Regex = """^system\s+([A-Z].*?)(?:\s+\((.*)\))?""".r
  val subsystemRegex: Regex = """^subsystem\s+([A-Z].*?)(?:\s+\((.*)\))?""".r
  val lineCommentsRegex: Regex = """^//.*""".r
  val multiLineCommentsRegex: Regex = """^/\*.*""".r
  val emptyLineRegex: Regex = """^\s*$""".r

  def parse(fileToAnalyze: String): ParsedDocument = {
    require(fileToAnalyze.nonEmpty, "File to analyze cannot be empty")
    val fileName = FileUtil.fileNameFromPath(fileToAnalyze)

    val lines = Utils.Control.using(Source.fromFile(fileToAnalyze)(Codec.UTF8)) {
      source => (for (line <- source.getLines()) yield line).toList
    }

    val parsedLines = lines.map(l => parseLine(l, fileName)).filter(_.isDefined).map(_.get)
    val documentName = FileUtil.fileNameFromPath(fileToAnalyze)

    ParsedDocument(
      documentName,
      fileToAnalyze,
      parsedLines.toSet,
      DocumentType.Lando
    )
  }

  def parseLine(line: String, documentName: String): Option[ParsedReference] = {
    cleanLine(line) match {
      case lineCommentsRegex() | multiLineCommentsRegex() | emptyLineRegex() => None
      case relationRegex(source, relationType, target) => Some(Relation("", line, documentName, DocumentType.Lando, ReferenceType.Relation, source, relationType, target))
      case componentRegex(name, acronym) => Some(ParsedRef(name, line, documentName, DocumentType.Lando, ReferenceType.Component, acronym))
      case systemRegex(name, acronym) => Some(ParsedRef(name, line, documentName, DocumentType.Lando, ReferenceType.System, acronym))
      case subsystemRegex(name, acronym) => Some(ParsedRef(name, line, documentName, DocumentType.Lando, ReferenceType.SubSystem, acronym))
      case _ => None
    }
  }

  def cleanLine(line: String): String = {
    line.trim().stripSuffix("{").stripSuffix("}").stripSuffix(":;").stripSuffix(";")
  }
}
