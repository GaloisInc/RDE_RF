package Generators

import Analyzers.{DocumentAnalyzer, LatexDocumentData}
import Formatter.{InlineFormatter, LatexSyntax, MarginFomatter}
import Report.{LatexGenerator, PaperLayout}
import Utils.FileUtil
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

class LatexGeneratorTest extends AnyFlatSpec with should.Matchers {

  "Latex" should "be in Path" in {
    LatexGenerator.checkLatexInPath() should be(true)
  }
  //
  //  "LatexGenerator" should "be able to generate A4 header" in {
  //    LatexGenerator.latexHeader(PaperLayout.A4) should be(
  //      """|\documentclass{article}
  //         |\usepackage[pdftex, colorlinks = true, linkcolor = blue, urlcolor = blue, bookmarks = false]{hyperref}
  //         |\usepackage[a4paper, margin=1in]{geometry}
  //        |\usepackage{listings}
  //        |\usepackage{url}
  //        |\usepackage{alltt}
  //        |\usepackage{amssymb}
  //        |\usepackage{amsthm}
  //        |\usepackage{xspace}
  //        |\usepackage{lstautogobble}
  //        |\usepackage{tcolorbox}
  //        |\usepackage{float}
  //        |\usepackage{xcolor}
  //        |\usepackage{graphicx}
  //        |\usepackage{todonotes}
  //        |\usepackage{varioref}
  //        |\usepackage{hyperref}
  //        |\usepackage{cleveref}
  //        |\usepackage{marginnote}
  //        |\maxdeadcycles=500
  //        |""".stripMargin)
  //  }
  //
  //  "LatexGenerator" should "be able to generate B4 header" in {
  //    LatexGenerator.latexHeader(PaperLayout.B4) should be(
  //      """\documentclass{article}
  //        |\\usepackage[pdftex, colorlinks = true, linkcolor = blue, urlcolor = blue, bookmarks = false]{hyperref}
  //        |\\usepackage[b4paper, marginparwidth=8cm, marginparsep=3mm, includemp, heightrounded, outer=1cm]{geometry}
  //        |\\usepackage{listings}
  //        |\\usepackage{url}
  //        |\\usepackage{alltt}
  //        |\\usepackage{amssymb}
  //        |\\usepackage{amsthm}
  //        |\\usepackage{xspace}
  //        |\\usepackage{lstautogobble}
  //        |\\usepackage{tcolorbox}
  //        |\\usepackage{float}
  //        |\\usepackage{xcolor}
  //        |\\usepackage{graphicx}
  //        |\\usepackage{todonotes}
  //        |\\usepackage{varioref}
  //        |\\usepackage{hyperref}
  //        |\\usepackage{cleveref}
  //        |\\usepackage{marginnote}
  //        |\maxdeadcycles=500
  //        |""".stripMargin)
  //  }

  "LatexGenerator" should "be able to generate footer" in {
    LatexSyntax.endDocument should be("\\end{document}")
  }

  "LatexGenerator" should "be able to generate environment" in {
    LatexGenerator.addContentInsideEnvironment(Array("content"), "alltt") should
      be(
        """|\begin{alltt}
           |content
           |\end{alltt}""".stripMargin)
  }

  "LatexGenerator" should "be able to generate itemize" in {
    LatexGenerator.generateList(List("content")) should
      be(
        """|\begin{itemize}
           |\item content
           |\end{itemize}""".stripMargin)
  }


  "LatexGenerator" should "be able to generate section" in {
    val sectionName = "Section"
    val section = LatexSyntax.generateSection(sectionName)
    section should be(
      s"""\\section{$sectionName}
         |\\label{sec:$sectionName}""".stripMargin)
  }

  "LatexGenerator" should "be able to generate section for two words" in {
    val sectionName = "Section TwoWords"
    LatexSyntax.generateSection(sectionName) should be(
      s"""\\section{$sectionName}
         |\\label{sec:Section_TwoWords}""".stripMargin)
  }

  "LatexGenerator" should "be able to build A4 Latex Document" in {
    val latexDocument = LatexGenerator.generateLatexDocument("", PaperLayout.A4)
    val tempFile = Files.createTempFile("test", ".tex")
    val filePath = Files.write(tempFile, latexDocument.getBytes(StandardCharsets.UTF_8))
    val latexFile = new File(filePath.toString)
    LatexGenerator.buildLatexFile(latexFile, false)
    latexFile.exists() should be(true)
    latexFile.delete()
  }

  "LatexGenerator" should "be able to build B4 Latex Document" in {
    val latexDocument = LatexGenerator.generateLatexDocument("", PaperLayout.B4)
    val tempFile = Files.createTempFile("test", ".tex")
    val filePath = Files.write(tempFile, latexDocument.getBytes(StandardCharsets.UTF_8))
    val latexFile = new File(filePath.toString)
    LatexGenerator.buildLatexFile(latexFile, false)
    latexFile.exists() should be(true)
    latexFile.delete()
  }

  "LatexGenerator" should "be able to generate A4 Latex Document from References" in {
    val destinationDirectory = getClass.getResource("../").getPath + "/A4"
    val directory = new File(destinationDirectory)
    if (!directory.exists) {
      directory.mkdir
      // If you require it to make the entire directory path including parents,
      // use directory.mkdirs(); here instead.
    }
    val latexName = "Test_SourceReport_A4"
    val latexDocumentData = LatexDocumentData(latexName, directory.getPath, PaperLayout.A4, new InlineFormatter())

    generateReport(latexDocumentData)
  }

  "LatexGenerator" should "be able to generate B4 Latex Document from References" in {
    val destinationDirectory = getClass.getResource("../").getPath + "/B4"
    val directory = new File(destinationDirectory)
    if (!directory.exists) {
      directory.mkdir
    }
    val latexName = "Test_SourceReport_B4"
    val latexDocumentData = LatexDocumentData(latexName, directory.getPath, PaperLayout.B4, new MarginFomatter())

    generateReport(latexDocumentData)
  }

  private def generateReport(latexDocumentData: LatexDocumentData): Unit = {
    val sysmlDocuments = getClass.getResource("../SysML").getPath
    val landoDocuments = getClass.getResource("../Lando").getPath
    val cryptolDocuments = getClass.getResource("../Cryptol").getPath
    val svDocuments = getClass.getResource("../SystemVerilog").getPath
    val bsvDocuments = getClass.getResource("../BSV").getPath

    val filesToAnalyze = FileUtil.getListOfFiles(sysmlDocuments).toArray ++
      FileUtil.getListOfFiles(landoDocuments).toArray ++
      FileUtil.getListOfFiles(cryptolDocuments).toArray ++
      FileUtil.getListOfFiles(svDocuments).toArray
      //FileUtil.getListOfFiles(bsvDocuments).toArray

    val referenceReport = DocumentAnalyzer.generateReport(filesToAnalyze, latexDocumentData, true)
    LatexGenerator.generateLatexReportOfSources(referenceReport)

    //Ensure that the file was created
    val latexFile = new File(latexDocumentData.folder + "/" + latexDocumentData.title + ".tex")
    latexFile.exists() should be(true)
    latexFile.delete()
  }
}
