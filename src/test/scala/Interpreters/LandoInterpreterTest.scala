package Interpreters

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class LandoInterpreterTest extends AnyFlatSpec with should.Matchers {

  "Lando" should "be in environment" in {
    LandoInterpreter.verifyLandoInPath should be(true)
  }



}
