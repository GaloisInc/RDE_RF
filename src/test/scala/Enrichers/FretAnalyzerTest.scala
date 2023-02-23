package Enrichers

import DocumentEnrichers.FRETDocumentEnricher
import Formatter.InlineFormatter
import Types.DocumentType
import Utils.TestUtilityFret
import org.scalatest.flatspec._
import org.scalatest.matchers._

class FretAnalyzerTest extends AnyFlatSpec with should.Matchers {
  private val documentAnalyser = new FRETDocumentEnricher(new InlineFormatter())
  private val expectedDocumentType = DocumentType.Fret

  "FretEnricher" should "be able to extract types from RTS_Requirements" in {
    val fileName = "RTS_Requirements"
    TestUtilityFret.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, numberOfRequirements = 28)
  }

  "FretEnricher" should "be able to extract types from Requirement_var" in {
    val fileName = "requirements_vars"
    TestUtilityFret.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, numberOfTypes = 136)
  }

  "FretEnricher" should "be able to extract types from Requirements" in {
    val fileName = "requirements"
    TestUtilityFret.checkExtractReferences(fileName, documentAnalyser, expectedDocumentType, numberOfRequirements = 104)
  }
}



