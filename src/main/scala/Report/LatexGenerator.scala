package Report

import Formatter.LatexSanitizer
import Report.ReportTypes.ReportReference
import Types.DocumentInfos.{CryptolDocumentInfo, DocumentInfo, LandoDocumentInfo, SysMLDocumentInfo}
import Types.DocumentType
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

  def generateList(list: List[String]): String = {
    val sb = new StringBuilder()
    sb.append(emptyLine)
    sb.append("\\begin{itemize}")
    for (item <- list) {
      sb.append(s"\\item $item")
      sb.append(emptyLine)
    }
    sb.append("\\end{itemize}")
    sb.append(emptyLine)
    sb.toString()
  }

  def buildLatexFile(latexFile: File, buildTwice: Boolean, removeAuxFiles: Boolean = true): Unit = {
    val fPath = latexFile.getAbsolutePath
    val cmd = s"""$latexBuildCmd ${fPath}"""
    val exitCode = Process(cmd).!
    assert(exitCode == 0, s"LaTeX build failed with exit code $exitCode")
    if (buildTwice) {
      val exitCode = Process(cmd).!
      assert(exitCode == 0, s"LaTeX build failed with exit code $exitCode")
    }

    if (removeAuxFiles) {
      val currentDirectory = new java.io.File(".")
      val auxFileTypes = Array[String]("aux", "log", "out", "toc", "lof", "lot", "fls", "fdb_latexmk")
      val auxFiles = currentDirectory.listFiles().filter(f => auxFileTypes.exists(f.getName.endsWith))
      auxFiles.foreach(_.delete())
    }
  }

  def includeListing(documentInfo: DocumentInfo): String = {
    s"""
       |\\lstinputlisting[language=${documentInfo.getLanguage},
       |label={lst:${documentInfo.getReferenceName}},
       |caption={Listing ${documentInfo.getCaption}}]
       |{${documentInfo.filePath}}""".stripMargin
  }


  lazy val listingAndDefaultCommands: String = {
    val latex = new mutable.StringBuilder()
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
    latex.toString()
  }

  def generateLatexDocument(content: String): String = {
    latexHeader + emptyLine + listingAndDefaultCommands + emptyLine + beginDocument + emptyLine + content + emptyLine + latexFooter
  }

  def generateSection(sectionName: String): String = {
    s"""\\section{${LatexSanitizer.sanitizeName(sectionName)}}
       |\\ label{sec:${LatexSanitizer.sanitizeReferenceName(sectionName)}}
       |""".stripMargin
  }

  def generateSubSection(name: String): String = {
    s"""\\subsection{${LatexSanitizer.sanitizeName(name)}}
       |\\label{subsec:${LatexSanitizer.sanitizeReferenceName(name)}}
       |""".stripMargin
  }

  def generateSubSubSection(name: String): String = {
    s"""\\subsubsection{${LatexSanitizer.sanitizeName(name)}}
       |\\label{subsubsec:${LatexSanitizer.sanitizeReferenceName(name)}}
       |""".stripMargin
  }

  def generateLatexReportOfSources(report: ReportReference): String = {
    val latexContent = new mutable.StringBuilder()

    latexContent.append(generateSection(report.title))

    latexContent.append(emptyLine)

    latexContent.append(generateSection("Lando Models"))

    report.landoDocuments.foreach(m => {
      //latexContent.append(generateSubSection(m.getCaption))
      latexContent.append(emptyLine)
      latexContent.append(includeListing(m))
      latexContent.append(emptyLine)
    })

    latexContent.append(generateSection("SysML Models"))
    latexContent.append(emptyLine)

    report.sysmlDocuments.foreach(m => {
      //latexContent.append(generateSubSection(m.getCaption))
      latexContent.append(emptyLine)
      latexContent.append(includeListing(m))
      latexContent.append(emptyLine)
    })

    latexContent.append(generateSection("Cryptol Specifications"))
    latexContent.append(emptyLine)

    report.cryptolDocuments.foreach(m => {
      //latexContent.append(generateSubSection(m.getCaption))
      latexContent.append(emptyLine)
      latexContent.append(includeListing(m))
      latexContent.append(emptyLine)
    })

    val latexDocument = generateLatexDocument(latexContent.toString())

    val filePath = Files.write(Paths.get(report.folder, s"${report.title}.tex"), latexDocument.getBytes(StandardCharsets.UTF_8))

    buildLatexFile(new File(filePath.toString), buildTwice = true)
    latexContent.toString()
  }

  val latexHeader: String = {
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

  val beginDocument: String = "\\begin{document}"

  val latexFooter: String = "\\end{document}"

  val emptyLine: String =
    """|
       |""".stripMargin

}










