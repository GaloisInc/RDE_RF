package Regex

import DocumentEnrichers.SysMLDocumentEnricher
import Formatter.InlineFormatter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.matching.Regex

class SysMLRegexTest extends AnyFlatSpec with should.Matchers {
  private val formatterType = new InlineFormatter()
  private val documentEnricher = new SysMLDocumentEnricher(formatterType)

  private def nullIfEmpty(s: String): String = if (s.isEmpty || s == "") null else s

  private def regexTester(line: String, regex: Regex,
                          acronymExp: String = "", nameExp: String = "", nameQuotedExp: String = "",
                          refinementSymbolExp: String = "", refinementNameExp: String = ""): Unit = {
    line matches regex.toString() should be(true)

    line match {
      case regex(acronym, name, nameQuoted, refinementSymbol, refinementName) =>
        nullIfEmpty(acronymExp) should be(acronym)
        nullIfEmpty(nameExp) should be(name)
        nullIfEmpty(nameQuotedExp) should be(nameQuoted)
        nullIfEmpty(refinementSymbolExp) should be(refinementSymbol)
        nullIfEmpty(refinementNameExp) should be(refinementName)
      case regex(name, nameQuoted, refinementSymbol, refinementName) =>
        nullIfEmpty(nameExp) should be(name)
        nullIfEmpty(nameQuotedExp) should be(nameQuoted)
        nullIfEmpty(refinementSymbolExp) should be(refinementSymbol)
        nullIfEmpty(refinementNameExp) should be(refinementName)
      case regex(acronym, name, nameQuoted) =>
        nullIfEmpty(acronymExp) should be(acronym)
        nullIfEmpty(nameExp) should be(name)
        nullIfEmpty(nameQuotedExp) should be(nameQuoted)
      case _ => fail(s"Regex (${regex.toString()}) did not match: $line")
    }
  }


  "RegexExperiment" should "match Items" in {
    val itemLine1 = "abstract item id <BISL> 'Behavioral Interface Specification Language'"
    val itemLine2 = "abstract item def SAT"
    val itemLine3 = "abstract item def 'Soft Core'"
    val itemLine4 = "abstract item def Refinement:> Relationship"
    val componentRegex: Regex = documentEnricher.componentRegex

    regexTester(itemLine1, componentRegex, "BISL", nameQuotedExp = "Behavioral Interface Specification Language")
    regexTester(itemLine2, componentRegex, nameExp = "SAT")
    regexTester(itemLine3, componentRegex, nameQuotedExp = "Soft Core")
    regexTester(itemLine4, componentRegex, nameExp = "Refinement", refinementSymbolExp = ":>", refinementNameExp = "Relationship")
  }


  "RegexExperiment" should "match Requirements" in {
    val requirement1 = "requirement 'Requirements Consistency' : 'NRC Characteristic'"
    val requirement2 = "requirement 'Instrumentation Independence' : 'NRC Characteristic'"
    val requirement3 = "requirement def 'Project Requirements'"
    val requirement4 = "requirement def id <FRET> 'FRET Requirements'"
    val requirementRegex = documentEnricher.requirementRegex

    regexTester(requirement1, requirementRegex, nameQuotedExp = "Requirements Consistency", refinementSymbolExp = ":",
      refinementNameExp = "'NRC Characteristic'")

    regexTester(requirement2, requirementRegex, nameQuotedExp = "Instrumentation Independence", refinementSymbolExp = ":",
      refinementNameExp = "'NRC Characteristic'")
    regexTester(requirement3, requirementRegex, nameQuotedExp = "Project Requirements")
    regexTester(requirement4, requirementRegex, acronymExp = "FRET", nameQuotedExp = "FRET Requirements")
  }


  "RegexExperiment" should "match Actions" in {
    val action1 = "action id <S> 'Set Setpoint' : UI_IA"
    val action2 = "action id 'Display Pressure' : UI_OA"
    val action3 = "action id 'Display Indication Of Channel in Bypass': UI_OA'"
    val action4 = "action id <Vote> 'Vote on Like Trips using Two-out-of-four Coincidence' : IA'"

    regexTester(action1, documentEnricher.actionRegex, acronymExp = "S", nameQuotedExp = "Set Setpoint", refinementSymbolExp = ":",
      refinementNameExp = "UI_IA")

    regexTester(action2, documentEnricher.actionRegex, nameQuotedExp = "Display Pressure", refinementSymbolExp = ":", refinementNameExp = "UI_OA")

    regexTester(action3, documentEnricher.actionRegex, nameQuotedExp = "Display Indication Of Channel in Bypass", refinementSymbolExp = ":",
      refinementNameExp = "UI_OA'")

    regexTester(action4, documentEnricher.actionRegex, acronymExp = "Vote", nameQuotedExp = "Vote on Like Trips using Two-out-of-four Coincidence",
      refinementSymbolExp = ":", refinementNameExp = "IA'")
  }

  "RegexExperiment" should "match Systems" in {
    val line1 = "package id <Glossary> 'Project Glossary'"
    val line2 = "package id <Architecture> 'RTS Architecture'"
    val line3 = "package Sensor"
    val line4 = "package id <RTS_System_Arch> 'RTS System Architecture'"
    val systemRegex = documentEnricher.systemRegex

    regexTester(line1, systemRegex, acronymExp = "Glossary", nameQuotedExp = "Project Glossary")
    regexTester(line2, systemRegex, acronymExp = "Architecture", nameQuotedExp = "RTS Architecture")
    regexTester(line3, systemRegex, nameExp = "Sensor")
    regexTester(line4, systemRegex, acronymExp = "RTS_System_Arch", nameQuotedExp = "RTS System Architecture")
  }


  "RegexExperiment" should "match Views" in {
    val line1 = "view def 'NRC Assurance Customer View Definition'"
    val line2 = "view 'Galois Software Engineer View' : 'Galois Performer View Definition'"
    val line3 = "view 'Galois Assurance Engineer View' : 'Galois Performer View Definition'"
    val line4 = "view def 'NRC General Customer View Definition'"
    val viewRegex = documentEnricher.viewRegex

    regexTester(line1, viewRegex, nameQuotedExp = "NRC Assurance Customer View Definition")
    regexTester(line2, viewRegex, nameQuotedExp = "Galois Software Engineer View", refinementSymbolExp = ":",
      refinementNameExp = "'Galois Performer View Definition'")
    regexTester(line3, viewRegex, nameQuotedExp = "Galois Assurance Engineer View", refinementSymbolExp = ":",
      refinementNameExp = "'Galois Performer View Definition'")
    regexTester(line4, viewRegex, nameQuotedExp = "NRC General Customer View Definition")
  }


  "RegexExperiment" should "match Viewpoints" in {
    val line1 = "viewpoint 'NRC General Customer Viewpoint'"
    val line2 = "viewpoint 'Galois Principal View'"
    val line3 = "viewpoint 'General Party interested in Rigorous Digital Engineering'"
    val line4 = "viewpoint 'Galois Performer Viewpoint'"
    val viewPointRegex = documentEnricher.viewPointRegex

    regexTester(line1, viewPointRegex, nameQuotedExp = "NRC General Customer Viewpoint")
    regexTester(line2, viewPointRegex, nameQuotedExp = "Galois Principal View")
    regexTester(line3, viewPointRegex, nameQuotedExp = "General Party interested in Rigorous Digital Engineering")
    regexTester(line4, viewPointRegex, nameQuotedExp = "Galois Performer Viewpoint")
  }


  "RegexExperiment" should "match SubSystems" in {
    val line1 = "abstract part def Cryptol"
    val line2 = "part def id <VCC> 'Verifier for Concurrent C'"
    val line3 = "abstract part def id <LF> 'Logical Framework'"
    val line4 = "abstract part def 'System Specification' :> Specification"
    val line5 = "part def <CFSM> 'Core Finite State Machine' :> FSM"
    val line6 = "part def <Programming_IO> 'Programming I/O' :> IO;"
    val line7 = "part def <UI_IO> 'UI I/O' :> IO;"
    val line8 = "part def <Debugging_IO> 'Debugging I/O' :> IO"
    val line9 = "part 'Voting Logic': 'Voting'[2]"
    val line10 = "part 'Actuator Logic': 'Actuator'[2]"
    val line11 = "part 'Actuator'[2];"
    val subsystemRegex = documentEnricher.subsystemRegex

    regexTester(line1, subsystemRegex, nameExp = "Cryptol")
    regexTester(line2, subsystemRegex, acronymExp = "VCC", nameQuotedExp = "Verifier for Concurrent C")
    regexTester(line3, subsystemRegex, acronymExp = "LF", nameQuotedExp = "Logical Framework")
    regexTester(line4, subsystemRegex, nameQuotedExp = "System Specification", refinementSymbolExp = ":>", refinementNameExp = "Specification")
    regexTester(line5, subsystemRegex, acronymExp = "CFSM", nameQuotedExp = "Core Finite State Machine", refinementSymbolExp = ":>",
      refinementNameExp = "FSM")
    regexTester(line6, subsystemRegex, acronymExp = "Programming_IO", nameQuotedExp = "Programming I/O", refinementSymbolExp = ":>",
      refinementNameExp = "IO")
    regexTester(line7, subsystemRegex, acronymExp = "UI_IO", nameQuotedExp = "UI I/O", refinementSymbolExp = ":>", refinementNameExp = "IO")
    regexTester(line8, subsystemRegex, acronymExp = "Debugging_IO", nameQuotedExp = "Debugging I/O", refinementSymbolExp = ":>",
      refinementNameExp = "IO")
    regexTester(line9, subsystemRegex, nameQuotedExp = "Voting Logic", refinementSymbolExp = ":", refinementNameExp = "'Voting'[2]")
    regexTester(line10, subsystemRegex, nameQuotedExp = "Actuator Logic", refinementSymbolExp = ":", refinementNameExp = "'Actuator'[2]")
    regexTester(line11, subsystemRegex, nameQuotedExp = "Actuator", refinementSymbolExp = "[", refinementNameExp = "2]")
  }

  "RegexExperiment" should "match use cases" in {
    val line1 = "use case '4b - Cause Instrumentation Unit 2 to Fail' : EB"
    val line2 = "use case '5a - Cause Temperature Demultiplexor 1 to Fail' : EB"
    val line3 = "use case 'Full Self-Test' : NB"
    val line4 = "use case def id ST 'Normal Behavior Under Self-Test'"
    val line5 = "use case def <NB> 'Normal Behavior'"
    val usecaseRegex = documentEnricher.usecaseRegex

    regexTester(line1, usecaseRegex, nameQuotedExp = "4b - Cause Instrumentation Unit 2 to Fail", refinementSymbolExp = ":", refinementNameExp = "EB")
    regexTester(line2, usecaseRegex, nameQuotedExp = "5a - Cause Temperature Demultiplexor 1 to Fail", refinementSymbolExp = ":", refinementNameExp = "EB")
    regexTester(line3, usecaseRegex, nameQuotedExp = "Full Self-Test", refinementSymbolExp = ":", refinementNameExp = "NB")
    //TODO: fix this regex
    //regexTester(line4, usecaseRegex, acronymExp = "ST", nameQuotedExp = "Normal Behavior Under Self-Test")
    regexTester(line5, usecaseRegex, acronymExp = "NB", nameQuotedExp = "Normal Behavior")
  }

  "RegexExperiment" should "match attributes" in {
    val line1 = "attribute def Description :> SPD"
    val line2 = "attribute def 'Author Description Scope Triple' :> SPD"
    val line3 = "attribute def 'Expression Description Pair' :> SPD"
    val line4 = "attribute def 'Concurrency Semantic Property' :> 'Semantic Property'"
    val line5 = "attribute locks: Locks;"
    val line6 = "attribute def Invariant :> 'Expression Description Pair';"
    val line7 = "attribute def <SP> 'Semantic Property';"

    val attributeRegex = documentEnricher.attributeRegex

    regexTester(line1, attributeRegex, nameExp = "Description", refinementSymbolExp = ":>", refinementNameExp = "SPD")
    regexTester(line2, attributeRegex, nameQuotedExp = "Author Description Scope Triple", refinementSymbolExp = ":>", refinementNameExp = "SPD")
    regexTester(line3, attributeRegex, nameQuotedExp = "Expression Description Pair", refinementSymbolExp = ":>", refinementNameExp = "SPD")
    regexTester(line4, attributeRegex, nameQuotedExp = "Concurrency Semantic Property", refinementSymbolExp = ":>", refinementNameExp = "'Semantic Property'")
    regexTester(line5, attributeRegex, nameExp = "locks", refinementSymbolExp = ":", refinementNameExp = "Locks")
    regexTester(line6, attributeRegex, nameExp = "Invariant", refinementSymbolExp = ":>", refinementNameExp = "'Expression Description Pair'")
    regexTester(line7, attributeRegex, acronymExp = "SP", nameQuotedExp = "Semantic Property")
  }

}
