package Generators

import Analyzers.DocumentAnalyzer
import Formatter.LatexSyntax
import Report.LatexGenerator
import Utils.FileUtil
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

class LatexGeneratorTest extends AnyFlatSpec with should.Matchers {

  "Latex" should "be in Path" in {
    LatexGenerator.checkLatexInPath() should be(true)
  }

  "LatexGenerator" should "be able to generate header" in {
    LatexGenerator.latexHeader should be(
      """\documentclass{article}
        |\usepackage[pdftex, colorlinks = true, linkcolor = blue, urlcolor = blue, bookmarks = false]{hyperref}
        |\usepackage{listings}
        |\usepackage{url}
        |\usepackage{alltt}
        |\usepackage{amssymb}
        |\usepackage{amsthm}
        |\usepackage{xspace}
        |\usepackage{lstautogobble}
        |\usepackage{tcolorbox}
        |\usepackage{float}
        |\usepackage{xcolor}
        |\usepackage{graphicx}
        |\usepackage{varioref}
        |\usepackage{hyperref}
        |\usepackage{cleveref}""".stripMargin)
  }

  "LatexGenerator" should "be able to generate footer" in {
    LatexSyntax.endDocument should be("\\end{document}")
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

  "LatexGenerator" should "be able to build Latex Document" in {
    val latexDocument = LatexGenerator.generateLatexDocument("")
    val tempFile = Files.createTempFile("test", ".tex")
    val filePath = Files.write(tempFile, latexDocument.getBytes(StandardCharsets.UTF_8))
    val latexFile = new File(filePath.toString)
    LatexGenerator.buildLatexFile(latexFile, false)
    latexFile.exists() should be(true)
    latexFile.delete()
  }

  "LatexGenerator" should "be able to generate Latex Document from References" in {
    val sysmlDocuments = getClass.getResource("../SysML").getPath
    val landoDocuments = getClass.getResource("../Lando").getPath
    val cryptolDocuments = getClass.getResource("../Cryptol").getPath

    val filesToAnalyze = FileUtil.getListOfFiles(sysmlDocuments).toArray
      ++ FileUtil.getListOfFiles(landoDocuments).toArray
      ++ FileUtil.getListOfFiles(cryptolDocuments).toArray

    val targetFolder = getClass.getResource("../").getPath
    val latexName = "test"
    val referenceReport = DocumentAnalyzer.generateReport(filesToAnalyze, latexName, targetFolder, true)

    LatexGenerator.generateLatexReportOfSources(referenceReport)

    //Ensure that the file was created

    val latexFile = new File(targetFolder + latexName + ".tex")
    latexFile.exists() should be(true)
    latexFile.delete()
  }
}
