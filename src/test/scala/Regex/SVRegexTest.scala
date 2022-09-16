package Regex

import DocumentEnrichers.SVDocumentEnricher
import Formatter.InlineFormatter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class SVRegexTest extends AnyFlatSpec with should.Matchers {
  private val formatterType = new InlineFormatter()
  private val documentEnricher = new SVDocumentEnricher(formatterType)


  "RegexExperiment" should "match modules" in {
    val line1 = "module Coincidence_2_4"
    val line2 = "module TemperatureLogic"
    val line3 = "module Actuate_D1"
    val moduleRegex = documentEnricher.subsystemRegex

    line1 matches moduleRegex.toString should be(true)
    line1 match {
      case moduleRegex(name) =>
        name should be("Coincidence_2_4")
      case _ => fail("Regex did not match")
    }

    line2 matches moduleRegex.toString should be(true)
    line2 match {
      case moduleRegex(name) =>
        name should be("TemperatureLogic")
      case _ => fail("Regex did not match")
    }

    line3 matches moduleRegex.toString should be(true)
    line3 match {
      case moduleRegex(name) =>
        name should be("Actuate_D1")
      case _ => fail("Regex did not match")
    }
  }
}
