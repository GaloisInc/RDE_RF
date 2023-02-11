package Interpreters

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class CryptolInterpreterTest extends AnyFlatSpec with should.Matchers {
  "Cryptol" should "be be in environment" in {
    CryptolInterpreter.toolInstalled should be(true)
  }

  it should "be able to load a module" in {
    val cryptolFilePath = getClass.getResource("../Cryptol/RTS.cry").getPath
    val result = CryptolInterpreter.interpret(cryptolFilePath)
    result.filePath should be(cryptolFilePath)
    result.documentName should be("RTS")
    result.getTypes.size should be(10)
    result.getProperties.size should be(6)
    result.getFunctions.size should be(18)
  }

  it should "be able to load the Utils module" in {
    val cryptolFilePath = getClass.getResource("../Cryptol/Utils.cry").getPath
    val result = CryptolInterpreter.interpret(cryptolFilePath)
    result.filePath should be(cryptolFilePath)
    result.documentName should be("Utils")
    result.getTypes.size should be(0)
    result.getProperties.size should be(0)
    result.getFunctions.size should be(1)
  }

  it should "be able to load the Actuator module" in {
    val cryptolFilePath = getClass.getResource("../Cryptol/Actuator.cry").getPath
    val result = CryptolInterpreter.interpret(cryptolFilePath)
    result.filePath should be(cryptolFilePath)
    result.documentName should be("Actuator")
    result.getTypes.size should be(3)
    result.getFunctions.size should be(3)
    result.getProperties.size should be(0)
  }

  it should "be able to load the ActuationUnit" in {
    val cryptolFilePath = getClass.getResource("../Cryptol/ActuationUnit.cry").getPath
    val result = CryptolInterpreter.interpret(cryptolFilePath)
    result.filePath should be(cryptolFilePath)
    result.documentName should be("ActuationUnit")
    result.getTypes.size should be(5)
    result.getProperties.size should be(5)
    result.getFunctions.size should be(9)
  }

  it should "be able to load the InstrumentationUnit" in {
    val cryptolFilePath = getClass.getResource("../Cryptol/InstrumentationUnit.cry").getPath
    val result = CryptolInterpreter.interpret(cryptolFilePath)
    result.filePath should be(cryptolFilePath)
    result.documentName should be("InstrumentationUnit")
    result.getTypes.size should be(10)
    result.getProperties.size should be(9)
    result.getFunctions.size should be(30)
  }

  it should "be able to verify all cryptol files" in {
    val instrumentationUnitFilePath = getClass.getResource("../Cryptol/InstrumentationUnit.cry").getPath
    val result = CryptolInterpreter.verifyProperties(instrumentationUnitFilePath)
    result should be(true)

    val actuationUnitFilePath = getClass.getResource("../Cryptol/ActuationUnit.cry").getPath
    val result2 = CryptolInterpreter.verifyProperties(actuationUnitFilePath)
    result2 should be(true)

    val actuatorFilePath = getClass.getResource("../Cryptol/Actuator.cry").getPath
    val result3 = CryptolInterpreter.verifyProperties(actuatorFilePath)
    result3 should be(true)

    val utilsFilePath = getClass.getResource("../Cryptol/Utils.cry").getPath
    val result4 = CryptolInterpreter.verifyProperties(utilsFilePath)
    result4 should be(true)

    val rtsFilePath = getClass.getResource("../Cryptol/RTS.cry").getPath
    val result5 = CryptolInterpreter.verifyProperties(rtsFilePath)
    result5 should be(true)
  }

  it should "be able to see that not all properties can be verified" in {
    val instrumentationUnitFilePath = getClass.getResource("../Changed_Cryptol/Utils.cry").getPath
    val result = CryptolInterpreter.verifyProperties(instrumentationUnitFilePath)
    result should be(false)
  }
}
