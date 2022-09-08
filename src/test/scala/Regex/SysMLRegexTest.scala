package Regex

import DocumentEnrichers.SysMLDocumentEnricher
import Formatter.InlineFormatter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class SysMLRegexTest extends AnyFlatSpec with should.Matchers {
  private val formatterType = InlineFormatter()
  private val documentEnricher = SysMLDocumentEnricher(formatterType)

  "RegexExperiment" should "match Items" in {
    val itemLine1 = "abstract item id BISL 'Behavioral Interface Specification Language'"
    val itemLine2 = "abstract item def SAT"
    val itemLine3 = "abstract item def 'Soft Core'"
    val itemLine4 = "abstract item def Refinement:> Relationship"
    val componentRegex = documentEnricher.componentRegex

    itemLine1 matches (componentRegex.toString) should be(true)
    itemLine1 match {
      case componentRegex(acronym, name, _, _) => {
        acronym should be("BISL")
        name should be("Behavioral Interface Specification Language")
      }
      case _ => fail("Regex did not match")
    }
    itemLine2 matches (componentRegex.toString) should be(true)
    itemLine2 match {
      case componentRegex(acronym, name, _, _) => {
        acronym should be("SAT")
        name should be(null)
      }
      case _ => fail("Regex did not match")
    }

    itemLine3 matches (componentRegex.toString) should be(true)

    itemLine3 match {
      case componentRegex(acronym, name, _, _) => {
        acronym should be("")
        name should be("Soft Core")
      }
      case _ => fail("Regex did not match")
    }

    itemLine4 matches (componentRegex.toString) should be(true)
    itemLine4 match {
      case componentRegex(acronym, name, symbol, part) => {
        acronym should be("Refinement")
        name should be(null)
        symbol should be(":>")
        part should be("Relationship")
      }
      case _ => fail("Regex did not match")
    }
  }


  "RegexExperiment" should "match Requirements" in {
    val requirement1 = "requirement 'Requirements Consistency' : 'NRC Characteristic'"
    val requirement2 = "requirement 'Instrumentation Independence' : 'NRC Characteristic'"
    val requirement3 = "requirement def 'Project Requirements'"
    val requirement4 = "requirement def id FRET 'FRET Requirements'"
    val requirementRegex = documentEnricher.requirementRegex

    requirement1 matches (requirementRegex.toString) should be(true)
    requirement1 match {
      case requirementRegex(acronym, name, symbol, part) => {
        acronym should be("")
        name should be("Requirements Consistency")
        symbol should be(":")
        part should be("'NRC Characteristic'")
      }
      case _ => fail("Regex did not match")
    }
    requirement2 matches (requirementRegex.toString) should be(true)
    requirement2 match {
      case requirementRegex(acronym, name, symbol, part) => {
        acronym should be("")
        name should be("Instrumentation Independence")
        symbol should be(":")
        part should be("'NRC Characteristic'")
      }
      case _ => fail("Regex did not match")
    }

    requirement3 matches (requirementRegex.toString) should be(true)

    requirement3 match {
      case requirementRegex(acronym, name, _, _) => {
        acronym should be("")
        name should be("Project Requirements")
      }
      case _ => fail("Regex did not match")
    }

    requirement4 matches (requirementRegex.toString) should be(true)
    requirement4 match {
      case requirementRegex(acronym, name, _, _) =>
        acronym should be("FRET")
        name should be("FRET Requirements")
      case _ => fail("Regex did not match")
    }
  }


  "RegexExperiment" should "match Actions" in {
    val action1 = "action id S 'Set Setpoint' : UI_IA"
    val action2 = "action id 'Display Pressure' : UI_OA"
    val action3 = "action id 'Display Indication Of Channel in Bypass': UI_OA'"
    val action4 = "action id Vote 'Vote on Like Trips using Two-out-of-four Coincidence' : IA'"
    val actionRegex = documentEnricher.actionRegex

    action1 matches (actionRegex.toString) should be(true)
    action1 match {
      case actionRegex(acronym, name, symbol, part) => {
        acronym should be("S")
        name should be("Set Setpoint")
        symbol should be(":")
        part should be("UI_IA")
      }
      case _ => fail("Regex did not match")
    }
    action2 matches (actionRegex.toString) should be(true)
    action2 match {
      case actionRegex(acronym, name, symbol, part) => {
        acronym should be("")
        name should be("Display Pressure")
        symbol should be(":")
        part should be("UI_OA")
      }
      case _ => fail("Regex did not match")
    }

    action3 matches (actionRegex.toString) should be(true)

    action3 match {
      case actionRegex(acronym, name, symbol, part) =>
        acronym should be("")
        name should be("Display Indication Of Channel in Bypass")
        symbol should be(":")
        part should be("UI_OA'")
      case _ => fail("Regex did not match")
    }

    action4 matches (actionRegex.toString) should be(true)
    action4 match {
      case actionRegex(acronym, name, symbol, part) =>
        acronym should be("Vote")
        name should be("Vote on Like Trips using Two-out-of-four Coincidence")
        symbol should be(":")
        part should be("IA'")
      case _ => fail("Regex did not match")
    }
  }

  "RegexExperiment" should "match Systems" in {
    val line1 = "package id Glossary 'Project Glossary'"
    val line2 = "package id Architecture 'RTS Architecture'"
    val line3 = "package Sensor"
    val line4 = "package id RTS_System_Arch 'RTS System Architecture'"
    val systemRegex = documentEnricher.systemRegex

    line1 matches systemRegex.toString should be(true)
    line1 match {
      case systemRegex(acronym, name) =>
        acronym should be("Glossary")
        name should be("Project Glossary")
      case _ => fail("Regex did not match")
    }
    line2 matches (systemRegex.toString) should be(true)
    line2 match {
      case systemRegex(acronym, name) =>
        acronym should be("Architecture")
        name should be("RTS Architecture")
      case _ => fail("Regex did not match")
    }

    line3 matches systemRegex.toString should be(true)
    line3 match {
      case systemRegex(acronym, name) =>
        acronym should be("Sensor")
        name should be(null)
      case _ => fail("Regex did not match")
    }

    line4 matches (systemRegex.toString) should be(true)
    line4 match {
      case systemRegex(acronym, name) =>
        acronym should be("RTS_System_Arch")
        name should be("RTS System Architecture")
      case _ => fail("Regex did not match")
    }
  }


  "RegexExperiment" should "match Views" in {
    val line1 = "view def 'NRC Assurance Customer View Definition'"
    val line2 = "view 'Galois Software Engineer View' : 'Galois Performer View Definition'"
    val line3 = "view 'Galois Assurance Engineer View' : 'Galois Performer View Definition'"
    val line4 = "view def 'NRC General Customer View Definition'"
    val viewRegex = documentEnricher.viewRegex

    line1 matches viewRegex.toString should be(true)
    line1 match {
      case viewRegex(name, reference) =>
        name should be("NRC Assurance Customer View Definition")
        reference should be(null)
      case _ => fail("Regex did not match")
    }

    line2 matches (viewRegex.toString) should be(true)
    line2 match {
      case viewRegex(name, reference) =>
        name should be("Galois Software Engineer View")
        reference should be("Galois Performer View Definition")
      case _ => fail("Regex did not match")
    }

    line3 matches viewRegex.toString should be(true)
    line3 match {
      case viewRegex(name, reference) =>
        name should be("Galois Assurance Engineer View")
        reference should be("Galois Performer View Definition")
      case _ => fail("Regex did not match")
    }

    line4 matches (viewRegex.toString) should be(true)
    line4 match {
      case viewRegex(name, reference) =>
        name should be("NRC General Customer View Definition")
        reference should be(null)
      case _ => fail("Regex did not match")
    }
  }


  "RegexExperiment" should "match Viewpoints" in {
    val line1 = "viewpoint 'NRC General Customer Viewpoint'"
    val line2 = "viewpoint 'Galois Principal View'"
    val line3 = "viewpoint 'General Party interested in Rigorous Digital Engineering'"
    val line4 = "viewpoint 'Galois Performer Viewpoint'"
    val viewPointRegex = documentEnricher.viewPointRegex

    line1 matches viewPointRegex.toString should be(true)
    line1 match {
      case viewPointRegex(name) =>
        name should be("NRC General Customer Viewpoint")
      case _ => fail("Regex did not match")
    }
    line2 matches (viewPointRegex.toString) should be(true)
    line2 match {
      case viewPointRegex(name) =>
        name should be("Galois Principal View")
      case _ => fail("Regex did not match")
    }

    line3 matches viewPointRegex.toString should be(true)
    line3 match {
      case viewPointRegex(name) =>
        name should be("General Party interested in Rigorous Digital Engineering")
      case _ => fail("Regex did not match")
    }

    line4 matches (viewPointRegex.toString) should be(true)
    line4 match {
      case viewPointRegex(name) =>
        name should be("Galois Performer Viewpoint")
      case _ => fail("Regex did not match")
    }
  }


  "RegexExperiment" should "match SubSystems" in {
    val line1 = "abstract part def Cryptol"
    val line2 = "part def id VCC 'Verifier for Concurrent C'"
    val line3 = "abstract part def id LF 'Logical Framework'"
    val line4 = "abstract part def 'System Specification' :> Specification"
    val subsystemRegex = documentEnricher.subsystemRegex

    line1 matches subsystemRegex.toString should be(true)
    line1 match {
      case subsystemRegex(acronym, name, _, _) =>
        acronym should be("Cryptol")
        name should be(null)
      case _ => fail("Regex did not match")
    }
    line2 matches subsystemRegex.toString should be(true)
    line2 match {
      case subsystemRegex(acronym, name, _, _) =>
        acronym should be("VCC")
        name should be("Verifier for Concurrent C")
      case _ => fail("Regex did not match")
    }

    line3 matches subsystemRegex.toString should be(true)
    line3 match {
      case subsystemRegex(acronym, name, _, _) =>
        acronym should be("LF")
        name should be("Logical Framework")
      case _ => fail("Regex did not match")
    }

    line4 matches (subsystemRegex.toString) should be(true)
    line4 match {
      case subsystemRegex(acronym, name, symbol, part) =>
        acronym should be("")
        name should be("System Specification")
        symbol should be(":>")
        part should be("Specification")
      case _ => fail("Regex did not match")
    }
  }

  "RegexExperiment" should "match use cases" in {
    val line1 = "use case '4b - Cause Instrumentation Unit 2 to Fail' : EB"
    val line2 = "use case '5a - Cause Temperature Demultiplexor 1 to Fail' : EB"
    val line3 = "use case 'Full Self-Test' : NB"
    val line4 = "use case def id ST 'Normal Behavior Under Self-Test'"
    val usecaseRegex = documentEnricher.usecaseRegex

    line1 matches usecaseRegex.toString should be(true)
    line1 match {
      case usecaseRegex(acronym, name, symbol, part) =>
        acronym should be("")
        name should be("4b - Cause Instrumentation Unit 2 to Fail")
        symbol should be(":")
        part should be("EB")
      case _ => fail("Regex did not match")
    }
    line2 matches usecaseRegex.toString should be(true)
    line2 match {
      case usecaseRegex(acronym, name, symbol, part) =>
        acronym should be("")
        name should be("5a - Cause Temperature Demultiplexor 1 to Fail")
        symbol should be(":")
        part should be("EB")
      case _ => fail("Regex did not match")
    }

    line3 matches usecaseRegex.toString should be(true)
    line3 match {
      case usecaseRegex(acronym, name, symbol, part) =>
        acronym should be("")
        name should be("Full Self-Test")
        symbol should be(":")
        part should be("NB")
      case _ => fail("Regex did not match")
    }

    line4 matches (usecaseRegex.toString) should be(true)
    line4 match {
      case usecaseRegex(acronym, name, symbol, part) =>
        acronym should be("ST")
        name should be("Normal Behavior Under Self-Test")
        symbol should be(null)
        part should be("")
      case _ => fail("Regex did not match")
    }
  }

  "RegexExperiment" should "match attributes" in {
    val line1 = "attribute def Description :> SPD"
    val line2 = "attribute def 'Author Description Scope Triple' :> SPD"
    val line3 = "attribute def 'Expression Description Pair' :> SPD"
    val line4 = "attribute def 'Concurrency Semantic Property' :> 'Semantic Property'"
    val line5 = "attribute"
    val attributeRegex = documentEnricher.attributeRegex

    line1 matches attributeRegex.toString should be(true)
    line1 match {
      case attributeRegex(acronym, name, symbol, part) =>
        acronym should be("Description")
        name should be(null)
        symbol should be(":>")
        part should be("SPD")
      case _ => fail("Regex did not match")
    }
    line2 matches attributeRegex.toString should be(true)
    line2 match {
      case attributeRegex(acronym, name, symbol, part) =>
        acronym should be("")
        name should be("Author Description Scope Triple")
        symbol should be(":>")
        part should be("SPD")
      case _ => fail("Regex did not match")
    }

    line3 matches attributeRegex.toString should be(true)
    line3 match {
      case attributeRegex(acronym, name, symbol, part) =>
        acronym should be("")
        name should be("Expression Description Pair")
        symbol should be(":>")
        part should be("SPD")
      case _ => fail("Regex did not match")
    }

    line4 matches (attributeRegex.toString) should be(true)
    line4 match {
      case attributeRegex(acronym, name, symbol, part) =>
        acronym should be("")
        name should be("Concurrency Semantic Property")
        symbol should be(":>")
        part should be("'Semantic Property'")
      case _ => fail("Regex did not match")
    }

    line5 matches attributeRegex.toString should be(false)
  }

}
