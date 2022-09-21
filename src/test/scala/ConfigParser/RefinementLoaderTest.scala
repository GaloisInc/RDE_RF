package ConfigParser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class RefinementLoaderTest extends AnyFlatSpec with should.Matchers {

  "RefinementLoader" should "load a refinement" in {
    val conf = getClass.getResource("../refinementExamples/refinement1.conf")
    val refinement = RefinementLoader.load(conf.getFile)
    assert(refinement.name == "Refinement 1")
    assert(refinement.explicit_refinements.values.flatten.size == 3)
  }

  "RefinementLoader" should "load a refinement with lots of refinements" in {
    val conf = getClass.getResource("../refinementExamples/Report.conf")
    val refinement = RefinementLoader.load(conf.getFile)
    assert(refinement.name == "Report")
    assert(refinement.implicit_refinements.values.flatten.size == 375)
    assert(refinement.explicit_refinements.isEmpty)

  }

  "RefinementLoader" should "load a generated refinement with lots of refinements" in {
    val conf = getClass.getResource("../refinementExamples/Report_Refinements.conf")
    val refinement = RefinementLoader.load(conf.getFile)
    assert(refinement.name == "Report_Refinements")
    assert(refinement.implicit_refinements.values.flatten.size == 311)
    assert(refinement.explicit_refinements.isEmpty)
  }

  "RefinementParser" should "parse parserRefinement" in {
    val results = RefinementParserSingleton.parse(RefinementParserSingleton.refinement, "file1.ref1 -> file2.ref2")
    results.successful should equal(true)
    val refinementModel = results.get
    refinementModel.srcRef.ref should equal("ref1")
    refinementModel.srcRef.file should equal("file1")
    refinementModel.trgRef.ref should equal("ref2")
    refinementModel.trgRef.file should equal("file2")
  }


  it should "parse tricky refinement" in {
    val results = RefinementParserSingleton.parse(RefinementParserSingleton.refinement, "file_with_underscore.x[4] -> file-with-hyphen.x[4]")

    results.successful should equal(true)
    val refinement = results.get
    refinement.srcRef.file should equal("file_with_underscore")
    refinement.srcRef.ref should equal("x[4]")
    refinement.trgRef.file should equal("file-with-hyphen")
    refinement.trgRef.ref should equal("x[4]")
  }

  it should "parse tricky refinement with space" in {
    val results = RefinementParserSingleton.parse(RefinementParserSingleton.refinement, "file_with_underscore.Lattice ECP-5 FGPA Development Board -> file-with-hyphen.Lattice ECP-5 FGPA Development Board")

    results.successful should equal(true)
    val refinement = results.get
    refinement.srcRef.file should equal("file_with_underscore")
    refinement.srcRef.ref should equal("Lattice ECP-5 FGPA Development Board")
    refinement.trgRef.file should equal("file-with-hyphen")
    refinement.trgRef.ref should equal("Lattice ECP-5 FGPA Development Board")
  }

  it should "parse tricky refinement with space, brackets and ending in number" in {
    val results = RefinementParserSingleton.parse(RefinementParserSingleton.refinement, "file_with_underscore.Temperature Sensor 2 -> file-with-hyphen.Lattice ECP-5 FGPA Development Board[4]")

    results.successful should equal(true)
    val refinement = results.get
    refinement.srcRef.file should equal("file_with_underscore")
    refinement.srcRef.ref should equal("Temperature Sensor 2")
    refinement.trgRef.file should equal("file-with-hyphen")
    refinement.trgRef.ref should equal("Lattice ECP-5 FGPA Development Board[4]")
  }

  it should "parse refinement with / and -" in {
    val results = RefinementParserSingleton.parse(RefinementParserSingleton.refinement, "file_with_underscore.Debugging I/O -> file-with-hyphen.Vote on Like Trips using Two-out-of-four Coincidence")
    results.successful should equal(true)
    val refinement = results.get
    refinement.srcRef.file should equal("file_with_underscore")
    refinement.srcRef.ref should equal("Debugging I/O")
    refinement.trgRef.file should equal("file-with-hyphen")
    refinement.trgRef.ref should equal("Vote on Like Trips using Two-out-of-four Coincidence")
  }

  it should "parse refinement starting in letter-" in {
    val results = RefinementParserSingleton.parse(RefinementParserSingleton.refinement, "file_with_underscore.1a - Trip on Mock High Pressure Reading from that Pressure Sensor-> file-with-hyphen.Vote on Like Trips using Two-out-of-four Coincidence")
    results.successful should equal(true)
    val refinement = results.get
    refinement.srcRef.file should equal("file_with_underscore")
    refinement.srcRef.ref should equal("1a - Trip on Mock High Pressure Reading from that Pressure Sensor")
    refinement.trgRef.file should equal("file-with-hyphen")
    refinement.trgRef.ref should equal("Vote on Like Trips using Two-out-of-four Coincidence")
  }

  it should "parse refinement with one capital letter" in {
    val results = RefinementParserSingleton.parse(RefinementParserSingleton.refinement, "file_with_underscore.C -> file-with-hyphen.Vote on Like Trips using Two-out-of-four Coincidence")
    results.successful should equal(true)
    val refinement = results.get
    refinement.srcRef.file should equal("file_with_underscore")
    refinement.srcRef.ref should equal("C")
    refinement.trgRef.file should equal("file-with-hyphen")
    refinement.trgRef.ref should equal("Vote on Like Trips using Two-out-of-four Coincidence")
  }

  it should "parse refinement \\&" in {
    val results = RefinementParserSingleton.parse(RefinementParserSingleton.refinement, "acronyms.Digital Instrumentation \\& Control -> File.Ref")
    results.successful should equal(true)
    val refinement = results.get
    refinement.srcRef.file should equal("acronyms")
    refinement.srcRef.ref should equal("Digital Instrumentation \\& Control")
    refinement.trgRef.file should equal("File")
    refinement.trgRef.ref should equal("Ref")
  }

  it should "reject parsing of file starting with number" in {
    val results = RefinementParserSingleton.parse(RefinementParserSingleton.refinement, "9file_with_underscore.1a - Trip on Mock High Pressure Reading from that Pressure Sensor-> file-with-hyphen.Vote on Like Trips using Two-out-of-four Coincidence")
    results.successful should equal(false)
  }
}
