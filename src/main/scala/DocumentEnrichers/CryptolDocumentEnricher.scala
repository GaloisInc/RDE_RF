package DocumentEnrichers

import Formatter.LatexFormatter
import Interpreters.CryptolInterpreter
import Types.DocReference.DocReference
import Types.DocumentInfos.{CryptolDocumentInfo, DocumentInfo}
import Types.{DocumentType, ReferenceName, ReferenceType}
import Utils.{Control, FileUtil}

import scala.util.matching.Regex

class CryptolDocumentEnricher(override val formatterType: LatexFormatter,
                              override val skipTodos: Boolean = true) extends DocumentEnricher[CryptolDocumentInfo](formatterType, skipTodos) {
  // Reads a Document to create an object of the necessary information to enrich the document.
  //Regex to match
  val typeRegex: Regex = """^type\s+(.*?)\s*=\s*.*""".r
  val propertyRegex: Regex = """^property\s+(.*?)\s+.*""".r
  val eventRegex: Regex = """^(.*?)\s*:\s*.*\s*->.*""".r
  val importRegex: Regex = """^import\s+(\w*)\s*::\s*(\w*).*""".r

  def parseDocument(filePath: String): CryptolDocumentInfo = {
    require(filePath.nonEmpty, "filePath must not be empty")
    require(FileUtil.getFileType(filePath) == "cry", "filePath must be a Cryptol file")
    require(FileUtil.fileExists(filePath), "filePath must exist")

    if(CryptolInterpreter.toolInstalled){
      CryptolInterpreter.interpret(filePath)
    }else{
      val fileName = FileUtil.getFileName(filePath)
      val references = Control.extractReferences(filePath, (l: String) => transformReference(l, fileName))
      val types: Set[DocReference] = references.filter(_.getReferenceType == ReferenceType.Type)
      val properties: Set[DocReference] = references.filter(_.getReferenceType == ReferenceType.Requirement)
      val functions: Set[DocReference] = references.filter(_.getReferenceType == ReferenceType.Event)
      val imports: Set[DocReference] = Set.empty[DocReference]
      new CryptolDocumentInfo(fileName, filePath, imports, types, functions, properties)
    }
  }

  def formatLine(line: String, documentInfo: CryptolDocumentInfo): String = {
    val references = documentInfo.getAllReferences

    val relevantReferences = cleanString(line) match {
      case typeRegex(_) => references.filter(_.getReferenceType == ReferenceType.Type)
      case propertyRegex(_) => references.filter(_.getReferenceType == ReferenceType.Requirement)
      case eventRegex(_) =>references.filter(_.getReferenceType == ReferenceType.Event)
      case importRegex(_, _) => references.filter(_.getReferenceType == ReferenceType.Import)
      case _ => Set.empty[DocReference]
    }
    relevantReferences.find(ref => line.contains(ref.getName) && line.trim.replaceAll(" +", " ").contains(ref.originalLine)) match {
      case Some(reference) => reference.enrich(latexFormatter)
      case None => line
    }
  }

  def transformReference(line: String, fileName: String): Option[DocReference] = {
    cleanString(line) match{
      case typeRegex(name) => Some(new DocReference(fileName, ReferenceName(name), ReferenceType.Type, DocumentType.Cryptol, line))
      case propertyRegex(name) => Some(new DocReference(fileName, ReferenceName(name), ReferenceType.Requirement, DocumentType.Cryptol, line))
      case eventRegex(name) => Some(new DocReference(fileName, ReferenceName(name), ReferenceType.Event, DocumentType.Cryptol, line))
      case importRegex(name, _) => Some(new DocReference(fileName, ReferenceName(name), ReferenceType.Import, DocumentType.Cryptol, line))
      case _ => None
    }
  }

  def cleanString(str: String): String = {
    str.trim
  }
}
