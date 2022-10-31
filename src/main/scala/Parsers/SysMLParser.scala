package Parsers

import Parsers.ParserTypes.{ParsedDocument, ParsedRef, ParsedRefWithReferences, ParsedReference}
import Types.{DocumentType, ReferenceType}
import Utils.FileUtil

import scala.io.{Codec, Source}
import scala.util.matching.Regex

object SysMLParser extends Parser {
  //Regex to match
  val systemRegex: Regex = """^(?:package)\s*(?:def)?\s*(?:id)?\s*(\w*)?\s*(?:'(.*?)')?""".r
  val componentRegex: Regex = """^(?:abstract)?\s*(?:item)\s*(?:def)?\s*(?:id)?\s*(\w*)?\s*(?:'(.*?)')?(?:\s*(:>|:)\s*(.*))?""".r
  val subsystemRegex: Regex = """^(?:abstract)?\s*part\s*(?:def)?\s*(?:id)?\s*(\w*)?\s*(?:'(.*?)')?(?:\s*(:>|:)\s*(.*))?""".r
  val attributeRegex: Regex = """^(?:abstract)?\s*attribute\s*(?:def)?\s*(?:id)?\s*(?=.)\s*(\w*)?\s*(?:'(.*?)')?(?:\s*(:>|:)\s*(.*))?""".r
  val requirementRegex: Regex = """^requirement\s*(?:def)?\s*(?:id)?\s*(\w*)?\s*(?:'(.*?)')?\s*(?:(:>|:)?\s*(.*))?""".r
  val usecaseRegex: Regex = """^use case\s*(?:def)?\s*(?:id)?\s*(\w*)?\s*(?:'(.*?)')?\s*(?:(:>|:)?\s*(.*))?""".r
  val actionRegex: Regex = """^action\s*(?:def)?\s*(?:id)?\s*(\w*)?\s*(?:'(.*?)')?\s*(:>|:)?\s*(.*)""".r
  val importRegex: Regex = """^import\s*(?:def)?\s*(?:id)?\s*(\w*)?\s*('(.*?)')?""".r
  val viewRegex: Regex = """^view\s*(?:def)?\s*(?:id)?\s*(?:'(.*?)')(?:\s*:\s*(?:'(.*)'))?""".r
  val viewPointRegex: Regex = """^viewpoint\s*(?:def)?\s*(?:id)?\s*'(.*?)'""".r
  val connectionRegex: Regex = """^connect\s*(\.*)\s=to\s+(.*?)""".r
  val lineCommentsRegex: Regex = """^//.*""".r
  val multiLineCommentsRegex: Regex = """^/\*.*""".r
  val emptyLineRegex: Regex = """^\s*$""".r

  def parse(fileToAnalyze: String): ParsedDocument = {
    require(fileToAnalyze.nonEmpty, "File to analyze cannot be empty")
    val fileName = FileUtil.getFileName(fileToAnalyze)

    val lines = Utils.Control.using(Source.fromFile(fileToAnalyze)(Codec.UTF8)) {
      source => (for (line <- source.getLines()) yield line).toList
    }

    val parsedLines = lines.map(l => parseLine(l, fileName)).filter(_.isDefined).map(_.get)
    val documentName = FileUtil.getFileName(fileToAnalyze)

    ParsedDocument(
      documentName,
      fileToAnalyze,
      parsedLines.toSet,
      DocumentType.Lando
    )
  }

  def parseLine(line: String, documentName: String): Option[ParsedReference] = {
    def emptyIfNull(s: String): String = if (s == null) "" else s

    cleanLine(line) match {
      case lineCommentsRegex() | multiLineCommentsRegex() | emptyLineRegex() => None
      case systemRegex(acronym, name) => Some(ParsedRef(emptyIfNull(name), line, documentName, DocumentType.SysML, ReferenceType.System, emptyIfNull(acronym)))
      case componentRegex(acronym, name, symbol, parent) => Some(ParsedRefWithReferences(emptyIfNull(name), line, documentName, DocumentType.SysML, ReferenceType.Component, emptyIfNull(acronym), symbol, parent))
      case subsystemRegex(acronym, name, symbol, parent) => Some(ParsedRefWithReferences(emptyIfNull(name), line, documentName, DocumentType.SysML, ReferenceType.SubSystem, emptyIfNull(acronym), symbol, parent))
      case attributeRegex(acronym, name, symbol, parent) => Some(ParsedRefWithReferences(emptyIfNull(name), line, documentName, DocumentType.SysML, ReferenceType.Attribute, emptyIfNull(acronym), symbol, parent))
      case requirementRegex(acronym, name, symbol, parent) => Some(ParsedRefWithReferences(emptyIfNull(name), line, documentName, DocumentType.SysML, ReferenceType.Requirement, emptyIfNull(acronym), symbol, parent))
      case actionRegex(acronym, name, symbol, parent) => Some(ParsedRefWithReferences(emptyIfNull(name), line, documentName, DocumentType.SysML, ReferenceType.Event, emptyIfNull(acronym), symbol, parent))
      case usecaseRegex(acronym, name, symbol, parent) => Some(ParsedRefWithReferences(emptyIfNull(name), line, documentName, DocumentType.SysML, ReferenceType.Scenario, emptyIfNull(acronym), symbol, parent))
      case importRegex(name) => Some(ParsedRef(emptyIfNull(name), line, documentName, DocumentType.SysML, ReferenceType.Import, ""))
      case viewRegex(name, parent) => Some(ParsedRefWithReferences(emptyIfNull(name), line, documentName, DocumentType.SysML, ReferenceType.View, "", ":", parent))
      case viewPointRegex(name) => Some(ParsedRef(emptyIfNull(name), line, documentName, DocumentType.SysML, ReferenceType.ViewPoint, ""))
      case _ => None
    }
  }

  def cleanLine(line: String): String = {
    line.trim().stripSuffix("{").stripSuffix("}").stripSuffix(":;").stripSuffix(";")
  }
}
