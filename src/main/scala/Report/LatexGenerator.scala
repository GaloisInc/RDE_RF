package Report

import Formatter.LatexSyntax.{beginDocument, endDocument, generateSection}
import Report.PaperLayout.PaperLayout
import Report.ReportTypes.ReportReference
import Types.DocumentInfos.DocumentInfo
import Utils.FileUtil
import org.apache.logging.log4j.scala.Logging

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.sys.process.Process

object LatexGenerator extends Logging {
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
    logger.info(s"Building LaTeX file $fPath")
    val cmd = s"""$latexBuildCmd -output-directory=${latexFile.getParent} $fPath"""
    val pLog = new LatexProcessLogger()
    val exitCode = Process(cmd).! //.!(pLog)
    assert(exitCode == 0, s"LaTeX build failed with exit code $exitCode")
    if (buildTwice) {
      val exitCode = Process(cmd).!(pLog)
      logger.info(s"LaTeX build finished with exit code $exitCode")
      assert(exitCode == 0, s"LaTeX build failed with exit code $exitCode")
    }

    if (removeAuxFiles) {
      val currentDirectory = new File(".")
      val auxFileTypes = Array[String]("aux", "log", "out", "toc", "lof", "lot", "fls", "fdb_latexmk")
      deleteAuxLatexFiles(currentDirectory, auxFileTypes)
    }
  }

  private def deleteAuxLatexFiles(directory: File, fileTypesToDelete: Array[String]): Unit = {
    require(directory.isDirectory, "Not a directory")
    val auxFiles = directory.listFiles().filter(f => fileTypesToDelete.exists(f.getName.endsWith))
    auxFiles.foreach(_.delete())
  } ensuring(_ => directory.listFiles().map(f => FileUtil.getFileType(f.getName)).toSet.intersect(fileTypesToDelete.toSet).isEmpty, "Auxiliary files not deleted")

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
    latex.append(ListingFormatting.lobotFormatListing)
    latex.append(emptyLine)
    latex.append(ListingFormatting.cryptolFormatting)
    latex.append(emptyLine)
    latex.append(ListingFormatting.sysmlFormatting)
    latex.append(emptyLine)
    latex.append(ListingFormatting.svFormatting)
    latex.append(emptyLine)
    latex.append(ListingFormatting.bsvFormatting)
    latex.append(emptyLine)

    latex.toString()
  }

  def generateLatexDocument(content: String, title: String, paperLayout: PaperLayout): String = {
    require(title.nonEmpty, "Title must not be empty")
    latexHeader(paperLayout) + emptyLine + listingAndDefaultCommands + emptyLine + beginDocument(title) + emptyLine + content + emptyLine + endDocument
  }


  def generateLatexReportOfSources(report: ReportReference): String = {
    val latexContent = new mutable.StringBuilder()

    latexContent.append(includeListings("Lando Models", report.landoDocuments))
    latexContent.append(includeListings("SysML Models", report.sysmlDocuments))
    latexContent.append(includeListings("Cryptol Specifications", report.cryptolDocuments))
    latexContent.append(includeListings("SystemVerilog Implementations", report.svDocuments))
    latexContent.append(includeListings("BlueSpec Implementations", report.bsvDocuments))

    val latexDocument = generateLatexDocument(latexContent.toString(), report.title, report.layout)

    val reportFileName = report.title.replaceAll(" ", "_")

    val filePath = Files.write(Paths.get(report.folder, s"$reportFileName.tex"), latexDocument.getBytes(StandardCharsets.UTF_8))

    buildLatexFile(new File(filePath.toString), buildTwice = true)
    latexContent.toString()
  }


  def includeListings[Doc <: DocumentInfo](sectionName: String, documents: Array[Doc]): String = {
    val latexContent = new mutable.StringBuilder()
    latexContent.append(generateSection(sectionName))
    latexContent.append(emptyLine)
    documents.foreach(m => {
      latexContent.append(emptyLine)
      latexContent.append(includeListing(m))
      latexContent.append(emptyLine)
    })
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
      "\\title{}" ++
      "\\author{Documentation Enricher}" ++
      "\\date{\\today}" ++
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










