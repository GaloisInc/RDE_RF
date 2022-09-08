package Report

import Formatter.LatexSanitizer
import Types.DocumentInfos.{CryptolDocumentInfo, DocumentInfo, LandoDocumentInfo, SysMLDocumentInfo}
import Types.DocumentType
import Report.ReportTypes.{ReportReference}
import Utils.FileUtil

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.sys.process.*

object LatexGenerator {
  private val latexBuildCmd = "pdflatex"
  private val packages = Array[String]("listings", "url", "alltt", "amssymb", "amsthm", "xspace",
    "lstautogobble", "tcolorbox", "float", "xcolor", "graphicx", "varioref", "hyperref", "cleveref")

  def checkLatexInPath(): Boolean = {
    val path = System.getenv("PATH")
    assert(path != null || path.contains(latexBuildCmd), "LaTeX not found in PATH")
    true
  }

  def buildLatexFile(latexFile: File): Unit = {
    val fPath = latexFile.getAbsolutePath
    val cmd = s"""$latexBuildCmd ${fPath}"""
    val exitCode = Process(cmd).!
    assert(exitCode == 0, s"LaTeX build failed with exit code $exitCode")
  }

  def includeListing(documentInfo: DocumentInfo): String = {
    s"""
       |\\lstinputlisting[language=${documentInfo.getLanguage},
       |label={lst:${documentInfo.getReferenceName}},
       |caption={Listing ${documentInfo.getCaption}}]
       |{${documentInfo.filePath}}""".stripMargin
  }

  lazy val latexHeader: String = {
    val latex = new mutable.StringBuilder()
    latex.append(
      s"""\\documentclass{article}
         |\\usepackage[pdftex, colorlinks = true, linkcolor = blue, urlcolor = blue, bookmarks = false]{hyperref}""".stripMargin)

    val packagesString = packages.foldLeft("")((acc, p) => acc +
      s"""
         |\\usepackage{$p}""").stripMargin
    latex.append(packagesString)
    latex.toString()
  }

  lazy val beginDocument: String = "\\begin{document}"

  lazy val latexFooter: String = "\\end{document}"

  def generateSection(sectionName: String): String = {
    s"""\\section{$sectionName}
       |\\label{sec:${LatexSanitizer.sanitizeReferenceName(sectionName)}}""".stripMargin
  }

  def generateLatexReport(report: ReportReference): String = {
    val latex = new mutable.StringBuilder()
    latex.append(latexHeader)

    latex.append(emptyLine)
    latex.append(ListingFormatting.standardCommands)
    latex.append(emptyLine)
    latex.append(ListingFormatting.basicFormatListing)
    latex.append(emptyLine)
    latex.append(ListingFormatting.landoFormatting)
    latex.append(emptyLine)
    latex.append(ListingFormatting.cryptolFormatting)
    latex.append(emptyLine)
    latex.append(ListingFormatting.sysmlFormatting)

    latex.append(emptyLine)
    latex.append(beginDocument)
    latex.append(emptyLine)

    latex.append(generateSection(report.title))

    latex.append(emptyLine)

    latex.append(generateSection("Lando Models"))

    report.landoDocuments.foreach(m => {
      latex.append(includeListing(m))
      latex.append(emptyLine)
    })

    latex.append(generateSection("SysML Models"))
    latex.append(emptyLine)

    report.sysmlDocuments.foreach(m => {
      latex.append(includeListing(m))
      latex.append(emptyLine)
    })


    latex.append(generateSection("Cryptol Specifications"))
    latex.append(emptyLine)

    report.cryptolDocuments.foreach(m => {
      latex.append(includeListing(m))
      latex.append(emptyLine)
    })

    latex.append(latexFooter)

    val filePath = Files.write(Paths.get("main.tex"), latex.toString().getBytes(StandardCharsets.UTF_8))

    buildLatexFile(new File(filePath.toString))
    latex.toString()
  }


  private val emptyLine: String =
    """|
       |""".stripMargin

}










