package Generators

import ConfigParser.RefinementModel
import Formatter.{InlineFormatter, MarginFormatter}
import Report._
import Utils.LatexCompilationTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.io.File
import java.nio.file.Files

class LatexGeneratorTest extends AnyFlatSpec with should.Matchers with LatexCompilationTester {
  private val authorName: String = "TestAuthor"

  "Latex" should "be in Path" in {
    LatexGenerator.toolInstalled should be(true)
  }

  "LatexGenerator" should "be able to generate environment" in {
    Environment("alltt", List("content")).toLatex should
      be(
        """|\begin{alltt}
           |content
           |\end{alltt}""".stripMargin)
  }

  "LatexGenerator" should "be able to generate itemize" in {
    ListBlock(List("content"), "itemize").toLatex should
      be(
        """|\begin{itemize}
           |\item content
           |\end{itemize}""".stripMargin)
  }


  "LatexGenerator" should "be able to generate section" in {
    val sectionName = "Section"
    val section = LatexSection(sectionName, sectionName, List.empty[LatexElement])
    section.toLatex should be(
      s"""\\section{$sectionName}
         |\\label{sec:$sectionName}
         |""".stripMargin)
  }

  "LatexGenerator" should "be able to generate section for two words" in {
    val sectionName = "Section TwoWords"
    val section = LatexSection(sectionName, sectionName, List.empty[LatexElement])
    section.toLatex should be(
      s"""\\section{$sectionName}
         |\\label{sec:Section_TwoWords}
         |""".stripMargin)
  }

  "LatexGenerator" should "be able to generate subsection for two words" in {
    val sectionName = "Section TwoWords"
    val text = Text("text")
    val section = Subsection(sectionName, sectionName, List(text))
    section.toLatex should be(
      s"""\\subsection{$sectionName}
         |\\label{sec:Section_TwoWords}
         |${text.toLatex}""".stripMargin)
  }

  "LatexGenerator" should "be able to generate subsubsection for two words" in {
    val sectionName = "Section TwoWords"
    val text = Text("text")
    val section = Subsubsection(sectionName, sectionName, List(text))
    section.toLatex should be(
      s"""\\subsubsection{$sectionName}
         |\\label{sec:Section_TwoWords}
         |${text.toLatex}""".stripMargin)
  }


  "LatexGenerator" should "be able to build A4 Latex Document" in {
    val tempFile = Files.createTempDirectory("test")
    buildDocumentationReport(Set(Types.DocumentType.Lando), "title", PaperLayout.A4, new InlineFormatter(), Set.empty[RefinementModel], sortFiles = false, tempFile.toAbsolutePath.toString)
    // Delete temp file
    tempFile.toFile.delete()
  }

  "LatexGenerator" should "be able to build B4 Latex Document" in {
    val tempFile = Files.createTempDirectory("test")
    buildDocumentationReport(Set(Types.DocumentType.Lando), "title", PaperLayout.B4, new InlineFormatter(), Set.empty[RefinementModel], sortFiles = false, tempFile.toAbsolutePath.toString)
    // Delete temp file
    tempFile.toFile.delete()
  }

  "LatexGenerator" should "be able to generate A4 Latex Document from References" in {
    val fileTypes = Set(Types.DocumentType.Lando, Types.DocumentType.Cryptol, Types.DocumentType.Fret, Types.DocumentType.SV, Types.DocumentType.Lobot, Types.DocumentType.SysML)
    val destinationDirectory = getClass.getResource("../").getPath + "/A4"
    val directory = new File(destinationDirectory)
    if (!directory.exists) {
      directory.mkdir
      // If you require it to make the entire directory path including parents,
      // use directory.mkdirs(); here instead.
    }
    val latexName = "Test_SourceReport_A4"
    buildDocumentationReport(fileTypes, latexName, PaperLayout.A4, new InlineFormatter(), Set.empty[RefinementModel], sortFiles = false, directory.getPath)
  }

  "LatexGenerator" should "be able to generate B4 Latex Document from References" in {
    val fileTypes = Set(Types.DocumentType.Lando, Types.DocumentType.Cryptol, Types.DocumentType.Fret, Types.DocumentType.SV, Types.DocumentType.Lobot, Types.DocumentType.SysML)
    val destinationDirectory = getClass.getResource("../").getPath + "/B4"
    val directory = new File(destinationDirectory)
    if (!directory.exists) {
      directory.mkdir
    }
    val latexName = "Test_SourceReport_B4"
    buildDocumentationReport(fileTypes, latexName, PaperLayout.B4, new MarginFormatter(), Set.empty[RefinementModel], sortFiles = false, directory.getPath)
  }
}
