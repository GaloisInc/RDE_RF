package Regex

import DocumentEnrichers.CryptolDocumentEnricher
import Formatter.InlineFormatter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class CryptolRegexTest extends AnyFlatSpec with should.Matchers {
  private val formatterType = new InlineFormatter()
  private val documentEnricher = new CryptolDocumentEnricher(formatterType)

  "RegexExperiment" should "match types" in {
    val line1 = "type Actuation = Bit"
    val line2 = "type Mode      = Bit"
    val line3 = "type State     = [2]"
    val line4 = "type Actuator ="
    val typeRegex = documentEnricher.typeRegex

    line1 matches typeRegex.toString should be(true)
    line1 match {
      case typeRegex(name) => name should be("Actuation")
      case _ => fail("Regex did not match")
    }

    line2 matches typeRegex.toString should be(true)
    line2 match {
      case typeRegex(name) => name should be("Mode")
      case _ => fail("Regex did not match")
    }

    line3 matches typeRegex.toString should be(true)
    line3 match {
      case typeRegex(name) => name should be("State")
      case _ => fail("Regex did not match")
    }

    line4 matches typeRegex.toString should be(true)
    line4 match {
      case typeRegex(name) => name should be("Actuator")
      case _ => succeed
    }
  }


  "RegexExperiment" should "match Functions" in {
    val line1 = "SetInput: Actuation -> Actuator -> Actuator"
    val line2 = "SetManual: Actuation -> Actuator -> Actuator"
    val line3 = "ActuateActuator : [2]Actuation -> Actuation"
    val eventRegex = documentEnricher.eventRegex

    line1 matches (eventRegex.toString) should be(true)
    line1 match {
      case eventRegex(name) =>
        name should be("SetInput")
      case _ => fail("Regex did not match")
    }

    line2 matches (eventRegex.toString) should be(true)
    line2 match {
      case eventRegex(name) =>
        name should be("SetManual")
      case _ => fail("Regex did not match")
    }

    line3 matches (eventRegex.toString) should be(true)
    line3 match {
      case eventRegex(name) =>
        name should be("ActuateActuator")
      case _ => fail("Regex did not match")
    }
  }

  "RegexExperiment" should "match Properties" in {
    val line1 = "property instrumentation_reset ="
    val line2 = "property instrumentation_set_manual_trip (instr: InstrumentationUnit) ="
    val line3 = "property step_state_const (inp: Input) (instr: InstrumentationUnit) ="
    val regex = documentEnricher.propertyRegex

    line1 matches regex.toString should be(true)
    line1 match {
      case regex(name) =>
        name should be("instrumentation_reset")
      case _ => fail("Regex did not match")
    }

    line2 matches regex.toString should be(true)
    line2 match {
      case regex(name) =>
        name should be("instrumentation_set_manual_trip")
      case _ => fail("Regex did not match")
    }

    line3 matches regex.toString should be(true)
    line3 match {
      case regex(name) =>
        name should be("step_state_const")
      case _ => fail("Regex did not match")
    }
  }

}
