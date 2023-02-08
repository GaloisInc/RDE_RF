package Report

import Formatter.LatexSanitizer
import Report.PaperLayout.PaperLayout
import Types.DocumentInfos.DocumentInfo
import Types.LatexReferenceTypes

import java.nio.file.{Files, Paths}

sealed trait LatexElement {
  def toLatex: String

  require(toLatex.nonEmpty, "The latex string cannot be empty")
  protected def formatInsideListing(content: String): String = content

}

sealed trait referencable {
  def getLabel: String

  require(getLabel.nonEmpty, "Label cannot be empty")
}

final case class LatexDocument(
                                title: String,
                                author: String,
                                elements: List[LatexElement],
                                layout: PaperLayout.PaperLayout = PaperLayout.A4,
                                packages: List[String] = List("listings", "url", "alltt", "amssymb", "amsthm", "xspace",
                                  "lstautogobble", "tcolorbox", "float", "xcolor", "graphicx", "todonotes", "varioref", "hyperref", "cleveref", "marginnote"),
                                dependencyFiles: List[IncludedFile] = List.empty[IncludedFile],
                              ) extends LatexElement {

  require(title.nonEmpty, "Document title cannot be empty")
  require(author.nonEmpty, "Document author cannot be empty")
  require(packages.nonEmpty, "Document packages cannot be empty")
  require(elements.nonEmpty, "Document elements cannot be empty")

  override def toLatex: String = {
    val titleLine = s"\\title{${LatexSanitizer.sanitizeName(title)}}"
    val beginDoc = "\\begin{document}"
    val endDoc = "\\end{document}"
    val authorString = s"\\author{${LatexSanitizer.sanitizeName(author)}}"
    val body = elements.map(_.toLatex).mkString("\n")
    val packagesStr = packages.map(p => s"\\usepackage{$p}").mkString("\n")
    s"""
       |\\documentclass{article}
       |% To ensure that the references look good
       |\\usepackage[pdftex, colorlinks = true, linkcolor = blue, urlcolor = blue, bookmarks = false]{hyperref}
       |$titleLine
       |$authorString
       |\\date{\\today}
       |${extractLatexLayout(layout)}
       |$packagesStr
       |${dependencyFiles.map(_.toLatex).mkString("\n")}
       |%Needed for the margin notes to work
       |\\maxdeadcycles=500
       |$beginDoc
       |\\maketitle
       |\\tableofcontents
       |$body
       |$endDoc""".stripMargin
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
}

final case class IncludedFile(path: String) extends LatexElement {
  require(path.nonEmpty, "Included file path cannot be empty")
  require(path.endsWith(".tex"), "Included file must be a .tex file")
  require(Files.exists(Paths.get(path)), "Included file must exist")

  override def toLatex: String = {
    s"""\\input{$path}"""
  }
}

final case class LatexSection(
                               title: String,
                               label: String,
                               elements: List[LatexElement]
                             ) extends LatexElement with referencable {

  require(title.nonEmpty, "Section title cannot be empty")
  require(label.nonEmpty, "Section label cannot be empty")

  override def toLatex: String = {
    val section = s"\\section{$title}\n\\label{sec:$getLabel}"
    val body = elements.map(_.toLatex).mkString("\n")
    s"$section\n$body"
  }
  override def getLabel: String = LatexSanitizer.sanitizeReferenceName(label)
}

final case class Subsection(title: String,
                            label: String,
                            content: List[LatexElement]
                           ) extends LatexElement with referencable {
  require(title.nonEmpty, "Subsection title cannot be empty")
  require(label.nonEmpty, "Subsection label cannot be empty")
  require(content.nonEmpty, "Subsection content cannot be empty")

  def toLatex: String = {
    val contentString = content.map(_.toLatex).mkString("\n")
    s"""\\subsection{$getTitle}
       |\\label{$getLabel}
       |$contentString""".stripMargin
  }

  def getLabel: String = LatexSanitizer.sanitizeReferenceName(label)

  def getTitle: String = LatexSanitizer.sanitizeName(title)
}


final case class Subsubsection(title: String,
                               label: String,
                               content: List[LatexElement]
                              ) extends LatexElement with referencable {
  require(title.nonEmpty, "Subsubsection title cannot be empty")
  require(label.nonEmpty, "Subsubsection label cannot be empty")
  require(content.nonEmpty, "Subsubsection content cannot be empty")

  def toLatex: String = {
    val contentString = content.map(_.toLatex).mkString("\n")
    s"""\\subsubsection{$getTitle}
       |\\label{$getLabel}
       |$contentString""".stripMargin
  }

  def getLabel: String = LatexSanitizer.sanitizeReferenceName(label)

  private def getTitle: String = LatexSanitizer.sanitizeName(title)
}

final case class CodeBlock(documentInfo: DocumentInfo,
                           language: String,
                           label: String) extends LatexElement with referencable {
  require(language.nonEmpty, "Code block language cannot be empty")
  require(label.nonEmpty, "Code block label cannot be empty")

  def toLatex: String =
    s"""
       |\\lstinputlisting[${listingStyle(documentInfo.documentType)}=${documentInfo.getLanguage},
       |label={lst:${documentInfo.getReferenceName}},
       |caption={Listing ${documentInfo.getCaption}.}]
       |{${documentInfo.filePath}}""".stripMargin

  private def listingStyle(documentType: Types.DocumentType.Value): String = {
    documentType match {
      case Types.DocumentType.Lando => "language"
      case Types.DocumentType.Lobot => "language"
      case Types.DocumentType.SysML => "language"
      case Types.DocumentType.Cryptol => "language"
      case Types.DocumentType.Saw => "language"
      case Types.DocumentType.SV => "language"
      case Types.DocumentType.BSV => "language"
      case Types.DocumentType.C => "language"
      case _ => "language"
    }
  } ensuring((res: String) => res.equals("style") || res.equals("language"), "Listing style not correct")


  def getLabel: String = LatexSanitizer.sanitizeReferenceName(label)
}

final case class ListBlock(items: List[String], listEnvironment: String) extends LatexElement {
  require(items.nonEmpty, "List cannot be empty")
  require(listEnvironment.compareToIgnoreCase("itemize") == 0 || listEnvironment.compareToIgnoreCase("enumerate") == 0, "List environment must be either itemize or enumerate")

  def toLatex: String = {
    val itemsString = items.map(item => s"\\item ${LatexSanitizer.sanitizeName(item)}").mkString("\n")
    s"""\\begin{$listEnvironment}
       |$itemsString
       |\\end{$listEnvironment}""".stripMargin
  }
}

final case class Figure(caption: String,
                        label: String,
                        path: String) extends LatexElement with referencable {
  def toLatex: String = {
    s"""\\begin{figure}[H]
       |\\centering
       |\\includegraphics[width=\\textwidth]{$path}
       |\\caption{$LatexSanitizer.sanitizeName(caption)}
       |\\label{$getLabel}
       |\\end{figure}""".stripMargin
  }

  def getLabel: String = LatexSanitizer.sanitizeReferenceName(label)
}

final case class Environment(environment: String,
                             content: List[String]) extends LatexElement {
  require(environment.nonEmpty, "Environment cannot be empty")
  require(content.nonEmpty, "Environment content cannot be empty")

  def toLatex: String = {
    val contentString = content.mkString("\n")
    s"""\\begin{$environment}
       |$contentString
       |\\end{$environment}""".stripMargin
  }
}

final case class Label(label: String,
                       formatting: String => String = (s: String) => s
                      ) extends LatexElement with referencable {
  require(label.nonEmpty, "Label cannot be empty")

  def toLatex: String = {
    val latex = s"\\label{$label}"
    formatting(latex)
  }

  override def getLabel: String = label

  override protected def formatInsideListing(content: String): String = {
    formatting(content)
  }
}

final case class ClickableLink(reference: String,
                               nameOfReference: String,
                               referenceType: LatexReferenceTypes.Value,
                               formatting: String => String = (s: String) => s) extends LatexElement {
  require(reference.nonEmpty, "reference must not be empty")
  require(nameOfReference.nonEmpty, "nameOfReference must not be empty")

  def toLatex: String = {
    val sanitizedReference = LatexSanitizer.sanitizeName(nameOfReference)
    val latex = referenceType match {
      case LatexReferenceTypes.File => s"\\href{run./$reference}{$sanitizedReference}"
      case LatexReferenceTypes.Abstraction => s"\\abstractionLink{$reference}{$sanitizedReference}"
      case LatexReferenceTypes.Refinement => s"\\refinementLink{$reference}{$sanitizedReference}"
      case LatexReferenceTypes.Link => s"\\link{$reference}{$sanitizedReference}"
      case LatexReferenceTypes.CryptolProperty => s"\\script{$reference}{$sanitizedReference}"
      case LatexReferenceTypes.ConnectionArtifact => s"\\hyperref[$reference]{$sanitizedReference}"
    }
    formatting(latex)
  } ensuring(_.nonEmpty, "Latex must not be empty")

  override protected def formatInsideListing(content: String): String = {
    formatting(content)
  }
}

final case class LatexReference(reference: String, referenceType: String,
                                formatting: String => String = (s: String) => s) extends LatexElement {
  require(reference.nonEmpty, "Reference must not be empty")
  require(reference.matches("^[a-zA-Z0-9_/]*$"), "Reference must only contain alphanumeric characters and underscores but was " + reference)
  require(referenceType.compareToIgnoreCase("cref") == 0 || referenceType.compareToIgnoreCase("vref") == 0, "Reference type must be either cref or vref")

  def toLatex: String = {
    val latex = s"\\${referenceType.toLowerCase}{$reference}"
    formatting(latex)
  }

  override protected def formatInsideListing(content: String): String = {
    formatting(content)
  }
}


final case class Text(text: String,
                      formatting: String => String = (s: String) => s,
                      color: String = "") extends LatexElement {
  require(text.nonEmpty, "Text must not be empty")

  def toLatex: String = {
    val latex = LatexSanitizer.sanitizeName(text)
    val coloredString = if (color.nonEmpty) s"\\textcolor{$color}{$latex}" else latex
    formatting(coloredString)
  }

  override protected def formatInsideListing(content: String): String = {
    formatting(content)
  }
}

final case class Weblink(url: String, name: String, formatting: String => String = (s: String) => s) extends LatexElement {
  require(url.nonEmpty, "URL must not be empty")
  require(name.nonEmpty, "Name must not be empty")

  def toLatex: String = {
    val latex = s"\\href{$url}{$name}"
    formatting(latex)
  }

  override protected def formatInsideListing(content: String): String = {
    formatting(content)
  }
}