package Interpreters

import scala.sys.process._

import Types.DocumentInfos.CryptolDocumentInfo

object CryptolInterpreter {
  private val startCMD: String = "cryptol"
  private val exitCMD: String = ":quit"

  private def typeCmd(nameOfModule: String): String = {
    require(nameOfModule.endsWith(".cry"), "The file is not a cryptol file.")
    s":b $nameOfModule"
  }

  def ensureCryptolIsInPath: Boolean = {
    startCMD.! == 0
  }

  def interpret(cryptolModule: String): CryptolDocumentInfo = {
    require(cryptolModule.nonEmpty, "Filename is not specified.")
    require(cryptolModule.endsWith(".cry"), "The file is not a cryptol file.")

    // Start Cryptol Environment
    val startResponse = startCMD.!
    assert(startResponse == 0, "Cryptol is not in the path.")

    val interpreterResult = typeCmd(cryptolModule).!!
    val extractDocument: CryptolDocumentInfo = extractDocumentFromString(interpreterResult, cryptolModule, "")

    // Quit Cryptol Environment
    val closeResponse = exitCMD.!
    assert(closeResponse == 0)

    extractDocument
  }


  def extractDocumentFromString(interpretedResult: String, fileName: String, filePath: String): CryptolDocumentInfo = {
    // Should skip everything from the base library
    val types = "Type Synonyms"
    val primitiveTypes = "Primitive Types"
    val newTypes = "Newtypes"
    val properties = "Properties"
    val functions = "Symbols"

    // We assume that the built-in Cryptol functions come first.




    CryptolDocumentInfo(
      fileName,
      filePath,
      Set.empty,
      Set.empty,
      Set.empty,
      Set.empty,
    )

  }
}
