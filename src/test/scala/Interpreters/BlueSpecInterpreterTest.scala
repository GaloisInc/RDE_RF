package Interpreters

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class BlueSpecInterpreterTest extends AnyFlatSpec with should.Matchers {

  "BlueSpecCompiler" should "be in environment" in {
    BlueSpecInterpreter.ensureBlueSpecInPath shouldBe(true)
  }

  it should "compile actuation" in {
    val filePath = getClass.getResource("../BSV/Actuation.bsv").getPath
    BlueSpecInterpreter.isWellFormed(filePath) shouldBe(true)
  }

  it should "be able to generate verilog from file" in {
    val filePath = getClass.getResource("../BSV/Nerv_BVI.bsv").getPath
    BlueSpecInterpreter.generateVerilogFile(filePath) shouldBe(filePath.replace(".bsv", ".v"))
  }

  it should "be able to generate BluesimObject from file" in {
    val filePath = getClass.getResource("../BSV/Instrumentation.bsv").getPath
    BlueSpecInterpreter.generateBluesimObject(filePath) shouldBe (getClass.getResource("../BSV/Instrumentation.bo").getPath)
  }


}
