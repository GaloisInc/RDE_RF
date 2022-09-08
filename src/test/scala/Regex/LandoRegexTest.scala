package Regex

import DocumentEnrichers.LandoDocumentEnricher
import Formatter.InlineFormatter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class LandoRegexTest extends AnyFlatSpec with should.Matchers {
  private val formatterType = InlineFormatter()
  private val documentEnricher = LandoDocumentEnricher(formatterType)

  "RegexExperiment" should "match attributes" in {
    val line1 = "component Digital Instrumentation \\& Control (diandc)"
    val line2 = "component Communicating Sequential Processes (CSP)"
    val line3 = "component Application-Specific Integrated Circuit (ASIC)"
    val line4 = "component Application-Specific Integrated Circuit"
    val line5 = "component USB Mini Connector (USB-Mini)"

    val componentRegex = documentEnricher.componentRegex

    line1 matches componentRegex.toString should be(true)
    line1 match {
      case componentRegex(name, acronym) =>
        name should be("Digital Instrumentation \\& Control")
        acronym should be("diandc")
      case _ => fail("Regex did not match")
    }
    line2 matches componentRegex.toString should be(true)
    line2 match {
      case componentRegex(name, acronym) =>
        name should be("Communicating Sequential Processes")
        acronym should be("CSP")
      case _ => fail("Regex did not match")
    }

    line3 matches componentRegex.toString should be(true)
    line3 match {
      case componentRegex(name, acronym) =>
        name should be("Application-Specific Integrated Circuit")
        acronym should be("ASIC")
      case _ => fail("Regex did not match")
    }

    line4 matches componentRegex.toString should be(true)
    line4 match {
      case componentRegex(name, acronym) =>
        name should be("Application-Specific Integrated Circuit")
        acronym should be (null)
      case _ => succeed
    }

    line5 matches componentRegex.toString should be(true)
    line5 match {
      case componentRegex(name, acronym) =>
        name should be("USB Mini Connector")
        acronym should be("USB-Mini")
      case _ => fail("Regex did not match")
    }
  }

  "RegexExperiment" should "match subsystems" in {
    val line1 = "subsystem Physical Architecture (physarch)"
    val line2 = "subsystem RTS Hardware Artifacts (rts-hw)"
    val line3 = "subsystem RTS Hardware Artifacts"
    val line4 = "subsystem which contains the core computation and I/O components, and"
    val subsystemRegex = documentEnricher.subsystemRegex

    line1 matches subsystemRegex.toString should be(true)
    line1 match {
      case subsystemRegex(name, acronym) =>
        name should be("Physical Architecture")
        acronym should be("physarch")
      case _ => fail("Regex did not match")
    }
    line2 matches subsystemRegex.toString should be(true)
    line2 match {
      case subsystemRegex(name, acronym) =>
        name should be("RTS Hardware Artifacts")
        acronym should be("rts-hw")
      case _ => fail("Regex did not match")
    }

    line3 matches subsystemRegex.toString should be(true)
    line3 match {
      case subsystemRegex(name, acronym) =>
        name should be("RTS Hardware Artifacts")
        acronym should be(null)
      case _ => fail("Regex did not match")
    }

    line4 matches subsystemRegex.toString should be(false)
  }

  "RegexExperiment" should "match systems" in {
    val line1 = "system Reactor Trip System (RTS)"
    val systemRegex = documentEnricher.systemRegex

    line1 matches systemRegex.toString should be(true)
    line1 match {
      case systemRegex(name, acronym) =>
        name should be("Reactor Trip System")
        acronym should be("RTS")
      case _ => fail("Regex did not match")
    }
  }

  "RegexExperiment" should "match relations" in {
    val line1 = "relation RTS_System_Arch contains Root"
    val line2 = "relation RTS_System_Arch contains Actuation Logic"
    val line3 = "relation RTS_System_Arch contains Computation"
    val line4 = "relation FPGA Dev Board contains J2"
    val line5 = "relation Board client Debug-C"
    val line6 = "relation Board inherit PCB"
    val regex = documentEnricher.relationRegex

    line1 matches regex.toString should be(true)
    line1 match {
      case regex(source, relation, target) =>
        source should be("RTS_System_Arch")
        relation should be("contains")
        target should be("Root")
      case _ => fail("Regex did not match")
    }
    line2 matches regex.toString should be(true)
    line2 match {
      case regex(source, relation, target) =>
        source should be("RTS_System_Arch")
        relation should be("contains")
        target should be("Actuation Logic")
      case _ => fail("Regex did not match")
    }

    line3 matches regex.toString should be(true)
    line3 match {
      case regex(source, relation, target) =>
        source should be("RTS_System_Arch")
        relation should be("contains")
        target should be("Computation")
      case _ => fail("Regex did not match")
    }

    line4 matches regex.toString should be(true)
    line4 match {
      case regex(source, relation, target) =>
        source should be("FPGA Dev Board")
        relation should be("contains")
        target should be("J2")
      case _ => fail("Regex did not match")
    }

    line5 matches regex.toString should be(true)
    line5 match {
      case regex(source, relation, target) =>
        source should be("Board")
        relation should be("client")
        target should be("Debug-C")
      case _ => fail("Regex did not match")
    }

    line6 matches regex.toString should be(true)
    line6 match {
      case regex(source, relation, target) =>
        source should be("Board")
        relation should be("inherit")
        target should be("PCB")
      case _ => fail("Regex did not match")
    }
  }

}
