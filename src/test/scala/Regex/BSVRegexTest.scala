package Regex

import DocumentEnrichers.BSVDocumentEnricher
import Formatter.InlineFormatter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class BSVRegexTest extends AnyFlatSpec with should.Matchers {
  private val formatterType = new InlineFormatter()
  private val documentEnricher = new BSVDocumentEnricher(formatterType)

  "RegexExperiment" should "match modules" in {
    val line1 = "module mkActuationGenerated(Actuation_IFC)"
    val line2 = "module mkActuationGeneratedD0 (ActuationD0_IFC)"
    val line3 = "module mkActuationGeneratedD1 (ActuationD1_IFC)"
    val moduleRegex = documentEnricher.subsystemRegex

    line1 matches (moduleRegex.toString) should be(true)
    line1 match {
      case moduleRegex(name, argument) =>
        name should be("mkActuationGenerated")
        argument should be("Actuation_IFC")
      case _ => fail("Regex did not match")
    }

    line2 matches moduleRegex.toString should be(true)
    line2 match {
      case moduleRegex(name, argument) =>
        name should be("mkActuationGeneratedD0")
        argument should be("ActuationD0_IFC")
      case _ => fail("Regex did not match")
    }

    line3 matches moduleRegex.toString should be(true)
    line3 match {
      case moduleRegex(name, argument) =>
        name should be("mkActuationGeneratedD1")
        argument should be("ActuationD1_IFC")
      case _ => fail("Regex did not match")
    }
  }

  "RegexExperiment" should "match packages" in {
    val line1 = "package Nerv_BVI"
    val line2 = "package Nerv"
    val line3 = "package Actuation_Generated_BVI"
    val systemRegex = documentEnricher.systemRegex

    line1 matches (systemRegex.toString) should be(true)
    line1 match {
      case systemRegex(name) =>
        name should be("Nerv_BVI")
      case _ => fail("Regex did not match")
    }

    line2 matches (systemRegex.toString) should be(true)
    line2 match {
      case systemRegex(name) =>
        name should be("Nerv")
      case _ => fail("Regex did not match")
    }

    line3 matches (systemRegex.toString) should be(true)
    line3 match {
      case systemRegex(name) =>
        name should be("Actuation_Generated_BVI")
      case _ => fail("Regex did not match")
    }
  }


}
