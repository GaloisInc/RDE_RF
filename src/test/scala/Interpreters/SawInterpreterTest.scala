package Interpreters

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class SawInterpreterTest extends AnyFlatSpec with should.Matchers {

  "SAW" should "be in environment" in {
    SawInterpreter.verifySawInstalled shouldBe true
  }

  it should "be able to run and validate a simple file" in {
    val sawFile = getClass.getResource("../saw/SHA512_easy.saw").getPath

    val result = SawInterpreter.verifySawFile(sawFile)
    result shouldBe true
  }
}
