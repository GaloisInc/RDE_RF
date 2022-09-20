package Interpreters

import Types.DocReference.DocReference

import scala.sys.process._
import Types.DocumentInfos.CryptolDocumentInfo
import Types.{DocumentType, ReferenceName, ReferenceType}
import Utils.FileUtil

object CryptolInterpreter {
  private val cmd: String = "cryptol"

  private def typeCmd(nameOfModule: String): String = {
    require(nameOfModule.endsWith(".cry"), "The file is not a cryptol file.")
    s"-c :b $nameOfModule"
  }

  def ensureCryptolIsInPath: Boolean = {
    s"$cmd -v".! == 0
  }

  def interpret(fileToCryptolModule: String): CryptolDocumentInfo = {
    require(fileToCryptolModule.nonEmpty, "Filename is not specified.")
    require(fileToCryptolModule.endsWith(".cry"), "The file is not a cryptol file.")

    val interpreterCmd = s"$cmd ${typeCmd(fileToCryptolModule)}"
    val result = interpreterCmd.!!
    val extractDocument: CryptolDocumentInfo = extractDocumentFromString(result, fileToCryptolModule)

    extractDocument
  }


  def extractDocumentFromString(interpretedResult: String, filePath: String): CryptolDocumentInfo = {
    // Should skip everything from the base library
    val types = "Type Synonyms"
    val primitiveTypes = "Primitive Types"
    val newTypes = "Newtypes"
    val properties = "Properties"
    val functions = "Symbols"
    val fileName = FileUtil.getFileName(filePath)

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

    new CryptolDocumentInfo(
      fileName,
      filePath,
      Set.empty,
      typesOfModule.map{
        case line@typeRegex(name) => new DocReference(fileName, ReferenceName(name), ReferenceType.Type, DocumentType.Cryptol, line)
        case _ => throw new Exception("Regex did not match")
      }.toSet,
      functionsOfModule.map {
        case line@functionRegex(name) => new DocReference(fileName, ReferenceName(name), ReferenceType.Event, DocumentType.Cryptol, line)
        case _ => throw new Exception("Could not parse function")
      }.toSet,
      propertiesOfModule.map {
        case line@propertyRegex(name) => new DocReference(fileName, ReferenceName(name), ReferenceType.Requirement, DocumentType.Cryptol, line)
        case _ => throw new Exception("Could not parse property")
      }.toSet,
    )
  }

  def extractRegexFromList(strings: Array[String], moduleName: String, regex : String): Array[String] = {
    val stringOfInterest = strings.dropWhile(!_.contains(s"From $moduleName"))
      //The first three lines are not interesting
      .drop(3)
      //We want to stop when we reach the next module
      .takeWhile(_.nonEmpty)
      .filter(_ matches regex)
    stringOfInterest
  }
}
