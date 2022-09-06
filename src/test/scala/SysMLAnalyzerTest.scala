import DocumentEnrichers.SysMLDocumentEnricher
import Types.{ReferenceType, DocumentType}
import Utils.{Control, FileUtil}
import Formatter.InlineFormatter
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import java.io.File
import scala.collection.mutable
import scala.io.Source

class SysMLAnalyzerTest extends AnyFlatSpec with should.Matchers {
  private val formatterType = InlineFormatter()
  private val documentEnricher = SysMLDocumentEnricher(formatterType)
  private val expectedDocumentType = DocumentType.SysML
  private val resourceFolder = "SysML"

  "SysMLReader" should "to extract parts and items" in {
    TestUtility.checkExtractReferences("RTS_Glossary", documentEnricher, expectedDocumentType, resourceFolder, 1, 80, 66, 0, 0, 0, 0, 0)
  }

  "SysMLReader" should "to extract requirements" in {
    TestUtility.checkExtractReferences("RTS_Requirements", documentEnricher, expectedDocumentType, resourceFolder, 4, 0, 0, 17, 0, 0, 0, 0)
  }

  "SysMLReader" should "to extract packages" in {
    TestUtility.checkExtractReferences("HARDENS", documentEnricher, expectedDocumentType, resourceFolder, 7, 0, 0, 0, 0, 0, 0, 0)
  }

  "SysMLReader" should "to extract Scenarios" in {
    TestUtility.checkExtractReferences("RTS_Scenarios", documentEnricher, expectedDocumentType, resourceFolder, 4, 0, 1, 0, 39, 0, 0, 0)
  }

  "SysMLReader" should "to extract Views" in {
    TestUtility.checkExtractReferences("RTS_Viewpoints", documentEnricher, expectedDocumentType, resourceFolder, 1, 0, 0, 0, 0, 8, 0, 0)
  }

  "SysMLReader" should "to extract Actions" in {
    TestUtility.checkExtractReferences("RTS_Actions", documentEnricher, expectedDocumentType, resourceFolder, 4, 0, 0, 0, 0, 0, 20, 0)
  }
}



