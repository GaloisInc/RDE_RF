package Utils

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class FileUtilTest extends AnyFlatSpec with should.Matchers {
  it should "be able to extract file name" in {
    val filePath1 = "src/test/resources/test1.txt"
    val filePath2 = "src/test/resources/test2.txt"
    val filePath3 = "src/test/KAS/resources/test3.txt"
    FileUtil.fileNameFromPath(filePath1) should be("test1")
    FileUtil.fileNameFromPath(filePath2) should be("test2")
    FileUtil.fileNameFromPath(filePath3) should be("test3")
  }

  it should "be able to extract file type" in {
    val filePath1 = "src/test/resources/test1.txt"
    val filePath2 = "src/test/resources/test2.cry"
    val filePath3 = "src/test/KAS/resources/test3.tex"
    FileUtil.getFileType(filePath1) should be("txt")
    FileUtil.getFileType(filePath2) should be("cry")
    FileUtil.getFileType(filePath3) should be("tex")
  }

  it should "be able to " in {
    val path = "lando.el"
    FileUtil.getFileType(path) should be("el")
  }

  it should "be able to extract file path" in {
    val filePath1 = "src/test/resources/test1.txt"
    val filePath2 = "src/test/resources/test2.cry"
    val filePath3 = "src/test/KAS/resources/test3.tex"
    FileUtil.getDirectory(filePath1) should be("src/test/resources")
    FileUtil.getDirectory(filePath2) should be("src/test/resources")
    FileUtil.getDirectory(filePath3) should be("src/test/KAS/resources")
  }

  it should "be able to find source files" in {
    val directory = getClass.getResource("../lando").getPath
    FileUtil.findSourceFiles(directory, Set("cry")) should be(Array.empty[String])
    val numberOfExpectedFiles = 13
    FileUtil.findSourceFiles(directory, Set("lando")).length should be(numberOfExpectedFiles)
    FileUtil.findSourceFiles(directory, Set("lando", "cry")).length should be(numberOfExpectedFiles)
  }
}
