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
    val entries = list.foldLeft("")((s, item) => {
      s + (s"\\item $item") + emptyLine
    })
    "\\begin{itemize}" + emptyLine + entries + "\\end{itemize}"
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

  private val latexAuxFiles = Array[String]("aux", "log", "out", "toc", "lof", "lot", "fls", "fdb_latexmk")

  def buildLatexFile(latexFile: File, buildTwice: Boolean, removeAuxFiles: Boolean = true): Unit = {
    val fPath = latexFile.getAbsolutePath
    logger.info(s"Building LaTeX file $fPath")
    val currentDirectory = new File(latexFile.getParent)
    // Delete aux files to avoid conflicts
    deleteAuxLatexFiles(currentDirectory, latexAuxFiles)

    val cmd = s"""$latexBuildCmd -output-directory=${latexFile.getParent} $fPath"""
    val exitCode = Process(cmd).!
    assert(exitCode == 0, s"LaTeX build failed with exit code $exitCode")
    if (buildTwice) {
      val pLog = new LatexProcessLogger()
      val exitCode = Process(cmd).!(pLog)
      logger.info(s"LaTeX build finished with exit code $exitCode")
      assert(exitCode == 0, s"LaTeX build failed with exit code $exitCode")
    }

    if (removeAuxFiles) {
      deleteAuxLatexFiles(currentDirectory, latexAuxFiles)
    }
  }

  private def deleteAuxLatexFiles(directory: File, fileTypesToDelete: Array[String]): Unit = {
    require(directory.isDirectory, "Not a directory")
    val auxFiles = directory.listFiles().filter(f => fileTypesToDelete.exists(f.getName.endsWith))
    auxFiles.foreach(_.delete())
  } ensuring(_ => !directory.listFiles().exists(f => fileTypesToDelete.exists(f.getName.endsWith)))

  def includeListing(documentInfo: DocumentInfo): String = {
    require(FileUtil.fileExists(documentInfo.filePath), s"File ${documentInfo.filePath} does not exist")
    val style = listingStyle(documentInfo.documentType)
    s"""
       |\\lstinputlisting[$style=${documentInfo.getLanguage},
       |label={lst:${documentInfo.getReferenceName}},
       |caption={Listing ${documentInfo.getCaption}.}]
       |{${documentInfo.filePath}}""".stripMargin
  }

  def listingStyle(documentType: Types.DocumentType.Value): String = {
    documentType match {
      case Types.DocumentType.Lando => "language"
      case Types.DocumentType.Lobot => "language"
      case Types.DocumentType.SysML => "language"
      case Types.DocumentType.Cryptol => "language"
      case Types.DocumentType.Saw => "language"
      case Types.DocumentType.SV => "language"
      case Types.DocumentType.BSV => "language"
      case Types.DocumentType.C => "language"
    }
  } ensuring((res: String) => res.equals("style") || res.equals("language"), "Listing style not correct")


  def includeFigure(documentInfo: DocumentInfo): String = {
    s"""
       |\\begin{figure}[H]
       |\\centering
       |\\includegraphics[width=\\textwidth]{${documentInfo.filePath}}
       |\\caption{${documentInfo.getCaption}}
       |\\label{fig:${documentInfo.getReferenceName}}
       |\\end{figure}""".stripMargin
  }


  def listingAndDefaultCommands(folderName: String): String = {
    require(folderName.nonEmpty, "Folder name cannot be empty")
    val latex = new mutable.StringBuilder()
    latex.append(ListingFormatting.standardCommands)
    latex.append(emptyLine)
    latex.append(ListingFormatting.basicFormatListing)
    latex.append(emptyLine)
    latex.append(ListingFormatting.landoFormatting)
    latex.append(emptyLine)
    latex.append(ListingFormatting.cryptolFormatting)
    latex.append(emptyLine)
    latex.append(ListingFormatting.lobotFormatting)
    latex.append(emptyLine)
    latex.append(ListingFormatting.sysmlFormatting)
    latex.append(emptyLine)
    latex.append(ListingFormatting.sawFormatting)
    latex.append(emptyLine)
    latex.append(ListingFormatting.Verilog)
    latex.append(emptyLine)
    latex.append(ListingFormatting.cFormatting)
    latex.append(emptyLine)

    //To hide weird characters in the listing environments
    latex.append("""\lstset{showstringspaces=false}""")
    latex.append(emptyLine)

    val filePath = Files.write(Paths.get(folderName, "languageCommands.tex"),
      latex.toString().getBytes(StandardCharsets.UTF_8))

    s"""\\input{$filePath}"""
  }

  def generateLatexDocument(content: String, title: String, folderOfDocument: String, paperLayout: PaperLayout.Value): String = {
    require(title.nonEmpty, "Title must not be empty")
    require(folderOfDocument.nonEmpty, "Folder of document must not be empty")
    latexHeader(paperLayout) + emptyLine + listingAndDefaultCommands(folderOfDocument) + emptyLine + beginDocument(title) + emptyLine + content + emptyLine + endDocument
  }


  def generateLatexReportOfSources(report: ReportReference): String = {
    val latexContent = new mutable.StringBuilder()

    latexContent.append(includeListings("Lando Models", report.landoDocuments, report.folder))
    latexContent.append(includeListings("Lobot Specifications", report.lobotDocuments, report.folder))
    latexContent.append(includeListings("SysML Models", report.sysmlDocuments, report.folder))
    latexContent.append(includeListings("Cryptol Specifications", report.cryptolDocuments,report.folder))
    latexContent.append(includeListings("Saw Specifications", report.sawDocuments, report.folder))
    latexContent.append(includeListings("SystemVerilog Implementations", report.svDocuments, report.folder))
    latexContent.append(includeListings("BlueSpec Implementations", report.bsvDocuments, report.folder))
    latexContent.append(includeListings("C Implementations", report.cDocuments, report.folder))

    val latexDocument = generateLatexDocument(latexContent.toString(), report.title, report.folder, report.layout)

    val reportFileName = report.title.replaceAll(" ", "_")

    val filePath = Files.write(Paths.get(report.folder, s"$reportFileName.tex"), latexDocument.getBytes(StandardCharsets.UTF_8))

    buildLatexFile(new File(filePath.toString), buildTwice = true)
    latexContent.toString()
  }


  def includeListings[Doc <: DocumentInfo](sectionName: String,
                                           documents: Array[Doc],
                                           folder: String): String = {
    //require(documents.nonEmpty, "No documents to include in section " + sectionName)
    require(folder.nonEmpty, "File path must not be empty")
    require(sectionName.nonEmpty, "Section name must not be empty")

    val latexContent = new mutable.StringBuilder()
    latexContent.append(generateSection(sectionName))
    latexContent.append(emptyLine)
    documents.foreach(m => {
      latexContent.append(emptyLine)
      latexContent.append(includeListing(m))
      latexContent.append(emptyLine)
    })

    val sanitizedSectionName = sectionName.replaceAll(" ", "_")
    val filePath = Files.write(Paths.get(folder, s"$sanitizedSectionName.tex"),
      latexContent.toString().getBytes(StandardCharsets.UTF_8))


    s"""\\input{$filePath}
         |
         |""".stripMargin

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












