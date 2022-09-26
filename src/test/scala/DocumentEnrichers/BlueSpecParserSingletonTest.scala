package DocumentEnrichers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

/*
class BlueSpecParserSingletonTest extends AnyFlatSpec with should.Matchers {

  it should "be able to parse import statements" in {
    val input = "import ruleName::*;"
    val input2 = "import Nerv_BVI :: *;"

    val parsedResult = BlueSpecParserSingleton.parse(BlueSpecParserSingleton.blueSpecImportParsing, input)
    parsedResult.successful should be(true)

    val result = parsedResult.get
    result.importName should be("ruleName")

    val parsedResult2 = BlueSpecParserSingleton.parse(BlueSpecParserSingleton.blueSpecImportParsing, input2)
    parsedResult2.successful should be(true)

    val result2 = parsedResult2.get
    result2.importName should be("Nerv_BVI")
  }

  it should "be able to parse module statements" in {
    val input = "module mkInstrGeneratedIsChannelTripped (ChannelTripped_IFC);"

    val parsedResult = BlueSpecParserSingleton.parse(BlueSpecParserSingleton.moduleParsing, input)
    parsedResult.successful should be(true)

    val result = parsedResult.get
    result.name should be("mkInstrGeneratedIsChannelTripped")
    result.argument should be("ChannelTripped_IFC")

    val input2 = "module mkActuationGenerated(Actuation_IFC);" // no space between module name and opening parenthesis
    val parsedResult2 = BlueSpecParserSingleton.parse(BlueSpecParserSingleton.moduleParsing, input2)
    parsedResult2.successful should be(true)

    val result2 = parsedResult2.get
    result2.name should be("mkActuationGenerated")
    result2.argument should be("Actuation_IFC")
  }

  it should "be able to parse typeDef struct " in {
    val input = "typedef struct { Bit #(4) name1; Bit #(32) name2; } DmemWrite deriving (Bits, Eq, FShow);"
    val parsedResult = BlueSpecParserSingleton.parse(BlueSpecParserSingleton.typeStructParsing, input)
    parsedResult.successful should be(true)

    val result = parsedResult.get
    result.name should be("DmemWrite")
  }

  it should "be able to parse typeDef enum " in {
    val input = "typedef enum { REQ_I, PUSH_I, REQ_D, PUSH_D } DmemWrite;"
    val parsedResult = BlueSpecParserSingleton.parse(BlueSpecParserSingleton.typeEnumsParsing, input)
    parsedResult.successful should be(true)

    val result = parsedResult.get
    result.name should be("DmemWrite")
  }

  it should "be able to parse typeDef enum deriving something " in {
    val input = "typedef enum { REQ_I, PUSH_I, REQ_D, PUSH_D } DmemWrite deriving(Bits, Eq, FShow);"
    val parsedResult = BlueSpecParserSingleton.parse(BlueSpecParserSingleton.typeEnumsParsing, input)
    parsedResult.successful should be(true)

    val result = parsedResult.get
    result.name should be("DmemWrite")
  }

  it should "be able to parse an interface definition" in {
    val input = "interface ChannelTripped_IFC;"
    val parsedResult = BlueSpecParserSingleton.parse(BlueSpecParserSingleton.interfaceParsing, input)
    parsedResult.successful should be(true)

    val result = parsedResult.get
    result.name should be("ChannelTripped_IFC")
  }

  it should "be able to parse an interface definition with name" in {
    val input = "interface ChannelTripped_IFC name00;"
    val parsedResult = BlueSpecParserSingleton.parse(BlueSpecParserSingleton.interfaceParsing, input)
    parsedResult.successful should be(true)

    val result = parsedResult.get
    result.name should be("ChannelTripped_IFC")
    result.argument should be(Some("name00"))
  }

  it should "be able to parse a rule with no parameters" in {
    val input = "rule ruleName;"

    val parsedResult = BlueSpecParserSingleton.parse(BlueSpecParserSingleton.ruleParsing, input)
    parsedResult.successful should be(true)

    val result = parsedResult.get
    result.name should be("ruleName")
  }

  it should "be able to parse a rule with parameters" in {
    val input = "rule ruleName;"

    val parsedResult = BlueSpecParserSingleton.parse(BlueSpecParserSingleton.ruleParsing, input)
    parsedResult.successful should be(true)

    val result = parsedResult.get
    result.name should be("ruleName")
  }

  /*
  it should "be able to parse method with no parameters" in {
    val input = "method method1;"
    val parsedResult = BlueSpecParserSingleton.parse(BlueSpecParserSingleton.methodParsing, input)
    parsedResult.successful should be(true)

    val result = parsedResult.get
    result.name should be("method1")
    result.returnType should be(None)
    result.parameters should be(Nil)
  }

  it should "be able to parse method with parameters" in {
    val input = "method method1(Bool b);"
    val parsedResult = BlueSpecParserSingleton.parse(BlueSpecParserSingleton.methodParsing, input)
    parsedResult.successful should be(true)

    val result = parsedResult.get
    result.name should be("method1")
    result.returnType should be(None)
  }


  it should "be able to parse method with return type" in {
    val input = "method Action method1;"
    val parsedResult = BlueSpecParserSingleton.parse(BlueSpecParserSingleton.methodParsing, input)
    parsedResult.successful should be(true)

    val result = parsedResult.get
    result.name should be("method1")
    result.returnType should be(Some("Action"))
  }

  it should "be able to parse method with return type and parameters" in {
    val input = "method Action method1 (Bool b);"
    val parsedResult = BlueSpecParserSingleton.parse(BlueSpecParserSingleton.methodParsing, input)
    parsedResult.successful should be(true)

    val result = parsedResult.get
    result.name should be("method1")
    result.returnType should be(Some("Action"))
    result.parameters should be(Some("Bool b"))
  }

  it should "be able to parse type" in {
    val input = "Bit #(32)"
    val parsedResult = BlueSpecParserSingleton.parse(BlueSpecParserSingleton.typeParsing, input)

    parsedResult.successful should be(true)
    val result = parsedResult.get
  }

  it should "be able to parse type" in {
    val input = "Vector #(32, Bit #(32))"
    val parsedResult = BlueSpecParserSingleton.parse(BlueSpecParserSingleton.typeParsing, input)

    parsedResult.successful should be(true)
    val result = parsedResult.get
  }

   */

}

 */
