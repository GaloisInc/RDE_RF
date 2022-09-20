package Report

import Report.ReportTypes.{BlockComment, LanguageFormatting, Literate}
import Types.DocumentType

object ListingFormatting {
  lazy val standardCommands: String = {
    """
      |\newcommand{\lando}{\textsc{Lando}\xspace}
      |\newcommand{\lobot}{\textsc{Lobot}\xspace}
      |\newcommand{\fret}{\textsc{FRET}\xspace}
      |\newcommand{\rts}{\textsc{RTS}\xspace}
      |\newcommand{\rde}{\textsc{RDE}\xspace}
      |\newcommand{\acsl}{\textsc{ACSL}\xspace}
      |\newcommand{\sysml}{\textsc{SysMLv2}\xspace}
      |\newcommand{\cryptol}{\textsc{Cryptol}\xspace}
      |\newcommand{\saw}{\textsc{SAW}\xspace}
      |\definecolor{keywordcolor}{RGB}{157,0,129}
      |\newcommand{\link}[2]{{\color{blue}\href{#1}{#2}}}
      |\newcommand{\script}[2]{{\color{purple}\href{#1}{#2}}}
      |\newcommand{\abstractionLink}[2]{
        \hypersetup{linkcolor=red}
          \hyperref[#1]{#2}
        \hypersetup{linkcolor=blue}
      }
      |\newcommand{\refinementLink}[2]{
        \hypersetup{linkcolor=green}
           \hyperref[#1]{#2}
        \hypersetup{linkcolor=blue}
      }
      |\newcommand{\fileLink}[2]{
        \hypersetup{linkcolor=orange}
          \href{run:./#1}{#2}
        \hypersetup{linkcolor=blue}
      }
      |""".stripMargin
  }

  lazy val basicFormatListing: String = {
    """
      |\lstset{
      |basicstyle =\scriptsize\ttfamily,
      |columns = fullflexible,
      |frame = single,
      |float = H,
      |breaklines = true,
      |numbers = left,
      |numberstyle =\small\itshape,
      |stepnumber = 1,
      |escapeinside = {(@}{@)},
      |postbreak =\mbox{\textcolor{red}{$\hookrightarrow$\space}}
    }""".stripMargin
  }

  def lstFormattings(documentType: DocumentType.Value): LanguageFormatting = {
    val languageFormatting = documentType match {
      case DocumentType.Lando =>
        ReportTypes.LanguageFormatting("Lando",
          keywords = Array("system", "subsystem", "component", "relation", "contains", "inherit", "client", "events", "scenarios"),
          lineComment = "//",
          blockComment = BlockComment("/*", "*/"),
          literates = Array.empty[Literate]
        )
      case DocumentType.Cryptol =>
        LanguageFormatting(
          "Cryptol",
          keywords = Array("type", "private", "import", "module", "where", "property", "if", "then", "else", "as", "take", "drop", "zero", "sum", "elem"),
          lineComment = "//",
          blockComment = BlockComment("/*", "*/"),
          literates = Array(Literate("/\\\\", "$\\land$ "), Literate("\\\\/", "$\\lor$ "))
        )
      case DocumentType.SysML =>
        LanguageFormatting(
          "SysML",
          Array(
            "type", "private", "import", "abstract", "item", "def", "part",
            "port", "out", "in", "attribute", "Boolean", "String", "package", "enum",
            "requirement", "subject", "action", "alias", "connection", "for", "ref",
            "end", "view", "flow", "viewpoint", "inout", "stakeholder", "redefines", "connect", "actor", "objective"),
          "//",
          BlockComment("/*", "*/"),
          literates = Array(Literate("⊑", "$\\sqsubseteq$ "), Literate("use\\ case", "\\color{keywordcolor}\\bfseries use\\ case"))
        )
      case DocumentType.SV =>
        LanguageFormatting(
          "SV",
          Array("module", "private", "import", "method", "input", "output", "assign", "endmodule", "end", "if", "else", "begin", "endfunction", "function", "endtask", "task", "for", "while", "repeat", "case", "endcase", "default", "return", "break", "continue", "rand", "randc", "randcase", "randsequence", "forever", "wait", "disable", "fork", "join", "join_any", "join_none", "void", "null", "super", "this", "const", "local", "static", "protected", "virtual", "automatic", "randsequence", "randcase", "randc", "rand", "typedef", "enum", "struct", "union", "class", "interface", "package", "program", "property", "sequence"),
          "//",
          BlockComment("/*", "*/"),
          literates = Array.empty[Literate]
        )
      case DocumentType.BSV =>
        LanguageFormatting(
          "BSV",
          Array("module", "private", "import", "method", "input", "output", "assign", "endmodule", "end", "if", "else", "begin", "endfunction", "function", "endtask", "task", "for", "while", "repeat", "case", "endcase", "default", "return", "break", "continue", "rand", "randc", "randcase", "randsequence", "forever", "wait", "disable", "fork", "join", "join_any", "join_none", "void", "null", "super", "this", "const", "local", "static", "protected", "virtual", "automatic", "randsequence", "randcase", "randc", "rand", "typedef", "enum", "struct", "union", "class", "interface", "package", "program", "property", "sequence"),
          "//",
          BlockComment("(*", "*)"),
          literates = Array.empty[Literate]
        )
    }
    languageFormatting
  }

  private def buildLanguageFormatting(languageFormatting: LanguageFormatting): String =
    s"""\\lstdefinelanguage{${languageFormatting.languageName}}{
       |basicstyle=\\scriptsize\\ttfamily,
       |keywordstyle=\\color{keywordcolor}\\bfseries,
       |commentstyle=\\itshape,
       |comment = [l]{${languageFormatting.lineComment}},
       |morecomment = [s]{${languageFormatting.blockComment.startSymbol}}{${languageFormatting.blockComment.endSymbol}},
       |extendedchars=true,
       |keywords = {${languageFormatting.keywords.mkString(", ")}},
       ${languageFormatting.getLiterates}}""".stripMargin


  lazy val landoFormatting: String = {
    val formatting = lstFormattings(DocumentType.Lando)
    buildLanguageFormatting(formatting)
  }

  lazy val cryptolFormatting: String = {
    val formatting = lstFormattings(DocumentType.Cryptol)
    buildLanguageFormatting(formatting)
  }

  lazy val sysmlFormatting: String = {
    val formatting = lstFormattings(DocumentType.SysML)
    buildLanguageFormatting(formatting)
  }

  lazy val bsvFormatting: String = {
    val formatting = lstFormattings(DocumentType.BSV)
    buildLanguageFormatting(formatting)
  }

  lazy val svFormatting: String = {
    val formatting = lstFormattings(DocumentType.SV)
    buildLanguageFormatting(formatting)
  }
}
