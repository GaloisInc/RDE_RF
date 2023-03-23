package DocumentEnrichers

import Formatter.LatexFormatter
import Interpreters.CryptolInterpreter
import Specs.FileSpecs
import Types.DocReference.DocReference
import Types.DocumentInfos.CryptolDocumentInfo
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
    require(FileSpecs.fileChecks(Set(filePath), Set("cry", "icry")), "filePath must be a cryptol file")

    if (CryptolInterpreter.toolInstalled) {
      CryptolInterpreter.interpret(filePath)
    } else {
      val fileName = FileUtil.fileNameFromPath(filePath)
      val references = Control.extractReferences(filePath, (l: String, lineNo: Int) => transformReference(l, lineNo, fileName))
      val types: Set[DocReference] = references.filter(_.getReferenceType == ReferenceType.Type)
      val properties: Set[DocReference] = references.filter(_.getReferenceType == ReferenceType.Requirement)
      val functions: Set[DocReference] = references.filter(_.getReferenceType == ReferenceType.Event)
      val imports: Set[DocReference] = Set.empty[DocReference]
      CryptolDocumentInfo(fileName, filePath, imports, types, functions, properties)
    }
  }

  override def formatLine(line: String, documentInfo: CryptolDocumentInfo): String = {
    val references = documentInfo.getAllReferences

    val relevantReferences = cleanString(line) match {
      case typeRegex(_) => references.filter(_.getReferenceType == ReferenceType.Type)
      case propertyRegex(_) => references.filter(_.getReferenceType == ReferenceType.Requirement)
      case eventRegex(_) => references.filter(_.getReferenceType == ReferenceType.Event)
      case importRegex(_, _) => references.filter(_.getReferenceType == ReferenceType.Import)
      case _ => Set.empty[DocReference]
    }
    relevantReferences.find(ref => line.contains(ref.getName) && line.trim.replaceAll(" +", " ").contains(ref.originalLine)) match {
      case Some(reference) => reference.enrich(latexFormatter)
      case None => line
    }
  }

  def transformReference(line: String, lineNo: Int, fileName: String): Option[DocReference] = {
    cleanString(line) match {
      case typeRegex(name) => Some(new DocReference(fileName, lineNo, ReferenceName(name), ReferenceType.Type, DocumentType.Cryptol, line))
      case propertyRegex(name) => Some(new DocReference(fileName, lineNo, ReferenceName(name), ReferenceType.Requirement, DocumentType.Cryptol, line))
      case eventRegex(name) => Some(new DocReference(fileName, lineNo, ReferenceName(name), ReferenceType.Event, DocumentType.Cryptol, line))
      case importRegex(name, _) => Some(new DocReference(fileName, lineNo, ReferenceName(name), ReferenceType.Import, DocumentType.Cryptol, line))
      case _ => None
    }
  }

  private def cleanString(str: String): String = {
    str.trim
  }
}
