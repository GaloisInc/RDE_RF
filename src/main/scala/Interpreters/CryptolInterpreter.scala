package Interpreters

import Specs.FileSpecs
import Types.DocReference.DocReference
import Types.DocumentInfos.CryptolDocumentInfo
import Types.{DocumentType, ReferenceName, ReferenceType}
import Utils.{CommandLineTool, FileUtil}
import org.apache.logging.log4j.scala.Logging

import scala.sys.process._

object CryptolInterpreter extends Logging with CommandLineTool {
  def command: String = "cryptol"
  def toolName: String = "Cryptol"

  private def check(filePath: String): Unit = {
    require(FileSpecs.fileChecks(Set(filePath), Set("cry", "icry")), "The file is not a cryptol file.")
    require(toolInstalled, "Cryptol is not in the path.")
  }

  private def typeCmd(nameOfModule: String): String = {
    check(nameOfModule)
    s"-c :b $nameOfModule"
  }

  private def verifyCmd(nameOfModule: String): String = {
    check(nameOfModule)
    s"-c :prove $nameOfModule"
  }

  private def verificationScriptCmd(nameOfScript: String): String = {
    check(nameOfScript)
    s"-b $nameOfScript"
  }

  private def wellformednessCmd(nameOfModule: String): String = {
    check(nameOfModule)
    val result = s"-c :b $nameOfModule"
    result
  }

  def interpret(fileToCryptolModule: String): CryptolDocumentInfo = {
    check(fileToCryptolModule)

    val interpreterCmd = s"$command ${typeCmd(fileToCryptolModule)}"
    val result = interpreterCmd.!!
    val extractDocument: CryptolDocumentInfo = extractDocumentFromString(result, fileToCryptolModule)

    extractDocument
  }

  def verifyProperties(fileToCryptolModule: String): Boolean = {
    require(fileToCryptolModule.nonEmpty, "Filename is not specified.")
    require(fileToCryptolModule.endsWith(".cry"), "The file is not a cryptol file.")
    require(toolInstalled, "Cryptol is not in the path.")

    val proveCmd = s"$command ${verifyCmd(fileToCryptolModule)}"
    val result = proveCmd.!!
    val lines = result.split("""\n""").map(_.trim)
    val numberOfProved = lines.count(_.startsWith("Q.E.D."))
    val numberOfProperties = lines.count(_.startsWith(":prove"))

    numberOfProved == numberOfProperties
  }

  def runVerificationScript(fileToCryptolModule: String): Boolean = {
    require(fileToCryptolModule.nonEmpty, "Filename is not specified.")
    require(fileToCryptolModule.endsWith(".icry"), "The file is not a cryptol file.")

    val proveCmd = s"$command ${verificationScriptCmd(fileToCryptolModule)}"
    val result = proveCmd.!!

    //TODO: parse result
    true
  }


  def verifyWellformednessCmd(fileToCryptolModule: String): Boolean = {
    require(fileToCryptolModule.nonEmpty, "Filename is not specified.")
    require(fileToCryptolModule.endsWith(".cry"), "The file is not a cryptol file.")
    require(toolInstalled, "Cryptol is not in the path.")

    logger.info(s"Verifying wellformedness of $fileToCryptolModule")
    val proveCmd = s"$command ${wellformednessCmd(fileToCryptolModule)}"
    val result = proveCmd.!!

    true
  }


  private def extractDocumentFromString(interpretedResult: String, filePath: String): CryptolDocumentInfo = {
    // Should skip everything from the base library
    val types = "Type Synonyms"
    val primitiveTypes = "Primitive Types"
    val newTypes = "Newtypes"
    val properties = "Properties"
    val functions = "Symbols"
    val fileName = FileUtil.fileNameFromPath(filePath)

    val lines = interpretedResult.split("""\n""").map(_.trim)
    val typesStart = lines.dropWhile(!_.contains(types)).takeWhile(!_.contains(primitiveTypes))

    val typeRegex = """^type (\w*) =.*""".r
    val typesOfModule = extractRegexFromList(typesStart, fileName, typeRegex.toString())

    val primitiveTypeStart = lines.dropWhile(!_.contains(primitiveTypes)).takeWhile(line => !line.contains(newTypes) && !line.contains(properties))

    val primitiveRegex = """^(\w*) :.*""".r
    val primitiveTypeOfModule = extractRegexFromList(primitiveTypeStart, fileName, primitiveRegex.toString())

    val propertiesStart = lines.dropWhile(!_.contains(properties)).takeWhile(!_.contains(functions))
    val propertyRegex = """^(\w*) :.*""".r
    val propertiesOfModule = extractRegexFromList(propertiesStart, fileName, propertyRegex.toString())

    val functionsStart = lines.dropWhile(!_.contains(functions))
    val functionRegex = """^(\w*) :.*""".r
    val functionsOfModule = extractRegexFromList(functionsStart, fileName, functionRegex.toString())

    val lineNo = 0

    CryptolDocumentInfo(
      fileName,
      filePath,
      Set.empty,
      typesOfModule.map {
        case line@typeRegex(name) => new DocReference(fileName, lineNo, ReferenceName(name), ReferenceType.Type, DocumentType.Cryptol, line)
        case _ => throw new Exception("Regex did not match")
      }.toSet,
      functionsOfModule.map {
        case line@functionRegex(name) => new DocReference(fileName, lineNo, ReferenceName(name), ReferenceType.Event, DocumentType.Cryptol, line)
        case _ => throw new Exception("Could not parse function")
      }.toSet,
      propertiesOfModule.map {
        case line@propertyRegex(name) => new DocReference(fileName, lineNo, ReferenceName(name), ReferenceType.Requirement, DocumentType.Cryptol, line)
        case _ => throw new Exception("Could not parse property")
      }.toSet,
    )
  }

  private def extractRegexFromList(strings: Array[String], moduleName: String, regex: String): Array[String] = {
    val stringOfInterest = strings.dropWhile(!_.contains(s"From $moduleName"))
      //The first three lines are not interesting
      .drop(3)
      //We want to stop when we reach the next module
      .takeWhile(_.nonEmpty)
      .filter(_ matches regex)
    stringOfInterest
  }
}
