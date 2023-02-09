package Report

import Formatter.LatexSanitizer
import Report.ReportTypes.ReportReference
import Types.DocumentInfos.DocumentInfo
import Utils.CommandLineTool
import org.apache.logging.log4j.scala.Logging

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.sys.process.Process

object LatexGenerator extends Logging with CommandLineTool {
  override val command = "pdflatex"
  override val toolName = "Latex - pdflatex"
  private val packages = Array[String]("listings", "url", "alltt", "amssymb", "amsthm", "xspace",
    "lstautogobble", "tcolorbox", "float", "xcolor", "graphicx", "todonotes", "varioref", "hyperref", "cleveref", "marginnote")
  private val latexAuxFiles = Array[String]("aux", "log", "out", "toc", "lof", "lot", "fls", "fdb_latexmk")
  private val reportAuthor = "Refinement Finder by Galois, Inc."

  def buildLatexFile(latexFile: File, buildTwice: Boolean, removeAuxFiles: Boolean = true): Unit = {
    val fPath = latexFile.getAbsolutePath
    logger.info(s"Building LaTeX file $fPath")
    val currentDirectory = new File(latexFile.getParent)
    // Delete aux files to avoid conflicts
    deleteAuxLatexFiles(currentDirectory, latexAuxFiles)

    val cmd = s"""$command -output-directory=${latexFile.getParent} $fPath"""
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
  } //ensuring(_ => !directory.listFiles().exists(f => fileTypesToDelete.exists(f.getName.endsWith)))


  def generateLatexReportOfSources(report: ReportReference): String = {
    val landoSection = includeListings("Lando Models", report.landoDocuments, report.folder)
    val lobotSection = includeListings("Lobot Specifications", report.lobotDocuments, report.folder)
    val sysmlSection = includeListings("SysML Models", report.sysmlDocuments, report.folder)
    val cryptolSection = includeListings("Cryptol Specifications", report.cryptolDocuments, report.folder)
    val sawSection = includeListings("Saw Specifications", report.sawDocuments, report.folder)
    val svSection = includeListings("SystemVerilog Implementations", report.svDocuments, report.folder)
    val bsvSection = includeListings("BlueSpec Implementations", report.bsvDocuments, report.folder)
    val cSection = includeListings("C Implementations", report.cDocuments, report.folder)

    val sections = List(landoSection, lobotSection, sysmlSection, cryptolSection, sawSection, svSection, bsvSection, cSection)
    val listingFormatting = ListingFormatting.createDefault(report.folder)
    val latexDocument = LatexDocument(report.title, report.author, sections, report.layout, packages.toList, List(listingFormatting))


    val reportFileName = report.title.replaceAll(" ", "_")

    val filePath = Files.write(Paths.get(report.folder, s"$reportFileName.tex"), latexDocument.toLatex.getBytes(StandardCharsets.UTF_8))

    buildLatexFile(new File(filePath.toString), buildTwice = true)
    filePath.toString
  }

  private def includeListings[T <: DocumentInfo[T]](sectionName: String,
                                                   documents: Array[T],
                                                   folder: String): IncludedFile = {
    require(folder.nonEmpty, "File path must not be empty")
    require(sectionName.nonEmpty, "Section name must not be empty")

    val codeBlocks = documents.map(m => CodeBlock(m, m.getLanguage, m.getReferenceName))
    val section = LatexSection(sectionName, sectionName, codeBlocks.toList)

    val sanitizedSectionName = LatexSanitizer.sanitizeReferenceName(sectionName)
    val filePath = Files.write(Paths.get(folder, s"$sanitizedSectionName.tex"),
      section.toLatex.getBytes(StandardCharsets.UTF_8))

    IncludedFile(filePath.toString)
  }
}












