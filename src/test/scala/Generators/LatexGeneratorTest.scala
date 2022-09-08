package Generators

import Analyzers.DocumentAnalyzer
import Report.LatexGenerator
import Utils.FileUtil
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.io.File

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
    LatexGenerator.latexFooter should be("\\end{document}")
  }

  "LatexGenerator" should "be able to generate section" in {
    val sectionName = "Section"
    val section = LatexGenerator.generateSection(sectionName)
    section should be(
      s"""\\section{$sectionName}
         |\\label{sec:$sectionName}""".stripMargin)
  }

  "LatexGenerator" should "be able to generate section for two words" in {
    val sectionName = "Section TwoWords"
    LatexGenerator.generateSection(sectionName) should be(
      s"""\\section{$sectionName}
         |\\label{sec:Section_TwoWords}""".stripMargin)
  }

  "LatexGenerator" should "be able to build Latex Document" in {
    val documents = getClass.getResource("../Latex").getPath
    val filesToAnalyze = FileUtil.getListOfFiles(documents).toArray
    val latexFilePath = filesToAnalyze.filter(path => FileUtil.getFileName(path) == "test").head
    val latexFile = new File(latexFilePath)
    LatexGenerator.buildLatexFile(latexFile)
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
    val referenceReport = DocumentAnalyzer.generateReport(filesToAnalyze, latexName, targetFolder)

    LatexGenerator.generateLatexReport(referenceReport)

    //Ensure that the file was created

    val latexFile = new File(targetFolder + latexName + ".tex")
    latexFile.exists() should be(true)
  }
}
