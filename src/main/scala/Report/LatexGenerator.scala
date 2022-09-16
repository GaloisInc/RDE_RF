package Report

import Formatter.LatexSyntax.{beginDocument, endDocument, generateSection}
import Report.PaperLayout.PaperLayout
import Report.ReportTypes.ReportReference
import Types.DocumentInfos.DocumentInfo

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.sys.process.Process

object LatexGenerator {
  private val latexBuildCmd = "pdflatex"
  private val packages = Array[String]("listings", "url", "alltt", "amssymb", "amsthm", "xspace",
    "lstautogobble", "tcolorbox", "float", "xcolor", "graphicx", "todonotes", "varioref", "hyperref", "cleveref", "marginnote")

  def checkLatexInPath(): Boolean = {
    val path = System.getenv("PATH")
    assert(path != null || path.contains(latexBuildCmd), "LaTeX not found in PATH")
    true
  }

  def generateList(list: List[String]): String = {
    val sb = new mutable.StringBuilder()
    sb.append("\\begin{itemize}")
    sb.append(emptyLine)
    for (item <- list) {
      sb.append(s"\\item $item")
      sb.append(emptyLine)
    }
    sb.append("\\end{itemize}")
    sb.toString()
  }

  def addContentInsideEnvironment(content: Array[String], environment: String): String = {
    val sb = new mutable.StringBuilder()
    sb.append("\\begin{" + environment + "}")
    sb.append(emptyLine)
    for (line <- content) {
      sb.append(line)
      sb.append(emptyLine)
    }
    sb.append("\\end{" + environment + "}")
    sb.toString()
  }

  def buildLatexFile(latexFile: File, buildTwice: Boolean, removeAuxFiles: Boolean = true): Unit = {
    val fPath = latexFile.getAbsolutePath
    val cmd = s"""$latexBuildCmd $fPath"""
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

  def generateLatexDocument(content: String, paperLayout: PaperLayout): String = {
    latexHeader(paperLayout) + emptyLine + listingAndDefaultCommands + emptyLine + beginDocument + emptyLine + content + emptyLine + endDocument
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

    val latexDocument = generateLatexDocument(latexContent.toString(), report.layout)

    val filePath = Files.write(Paths.get(report.folder, s"${report.title}.tex"), latexDocument.getBytes(StandardCharsets.UTF_8))

    buildLatexFile(new File(filePath.toString), buildTwice = true)
    latexContent.toString()
  }

  def latexHeader(paperLayout: PaperLayout): String = {
    val documentClass =
      s"""\\documentclass{article}
         |\\usepackage[pdftex, colorlinks = true, linkcolor = blue, urlcolor = blue, bookmarks = false]{hyperref}""".stripMargin

    val layout = extractLatexLayout(paperLayout)

    val packagesString = packages.foldLeft("")((acc, p) => acc +
      s"""
       |\\usepackage{$p}""").stripMargin


    documentClass ++
      layout ++
      packagesString ++
      emptyLine ++
      //Needed for the margin notes to work
      "\\maxdeadcycles=500" ++
      emptyLine
  }


  private def extractLatexLayout(paperLayout: PaperLayout): String = {
    paperLayout match {
      case PaperLayout.A4 =>
        s"""
           |\\usepackage[a4paper, margin=1in]{geometry}""".stripMargin
      case PaperLayout.B4 =>
        s"""
           |\\usepackage[b4paper, marginparwidth=8cm, marginparsep=3mm, includemp, heightrounded, outer=1cm]{geometry}""".stripMargin
    }
  }

  val emptyLine: String =
    """|
       |""".stripMargin

}

object PaperLayout extends Enumeration {
  type PaperLayout = Value
  val A4, B4 = Value
}










