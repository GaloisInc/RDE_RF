package Report

import Report.LatexGenerator.listingStyle
import Report.ReportTypes.{BlockComment, LanguageFormatting, Literate}
import Types.DocumentType

object ListingFormatting {

  val Verilog: String =
    """
      |\lstdefinelanguage{Verilog}
      |{
      |  basicstyle=\scriptsize\ttfamily,
      |  keywordstyle=\color{keywordcolor}\bfseries,
      |  identifierstyle=\color{black},
      |  commentstyle=\itshape,
      |  numbers=left,
      |  numberstyle=\tiny\color{black},
      |  numbersep=10pt,
      |  tabsize=8,
      |  numberstyle =\small\itshape,
      |  stepnumber = 1,
      |  escapeinside = {(@}{@)},
      |  postbreak =\mbox{\textcolor{red}{$\hookrightarrow$\space}},
      |  morekeywords ={
      |    begin,
      |    bit,
      |    break,
      |    case,
      |    continue,
      |    default,
      |    do,
      |    else,
      |    end,
      |    endcase,
      |    function,
      |    endfunction,
      |    endinterface,
      |    endmodule,
      |    endpackage,
      |    enum,
      |    export,
      |    extern,
      |    for,
      |    if,
      |    import,
      |    int,
      |    interface,
      |    local,
      |    localparam,
      |    matches,
      |    module,
      |    package,
      |    tagged,
      |    type,
      |    typedef,
      |    struct,
      |    union,
      |    void,
      |    %% Bluespec keywords
      |    action,
      |    endaction,
      |    actionvalue,
      |    endactionvalue,
      |    ancestor,
      |    deriving,
      |    instance,
      |    endinstance,
      |    let,
      |    return,
      |    match,
      |    method,
      |    endmethod,
      |    par,
      |    endpar,
      |    powered_by,
      |    provisos,
      |    rule,
      |    endrule,
      |    rules,
      |    endrules,
      |    seq,
      |    endseq,
      |    schedule,
      |    typeclass,
      |    endtypeclass,
      |    clock,
      |    reset,
      |    noreset,
      |    no_reset,
      |    clocked_by,
      |    reset_by,
      |    default_clock,
      |    default_reset,
      |    output_clock,
      |    output_reset,
      |    input_clock,
      |    input_reset,
      |    same_family,
      |    Action,
      |    ActionValue,
      |    Integer,
      |    Nat,
      |    Bit,
      |    UInt,
      |    Int,
      |    Bool,
      |    Maybe,
      |    String,
      |    Either,
      |    Rules,
      |    Module,
      |    Clock,
      |    Reset,
      |    Power,
      |    TAdd, TSub, TMul, TDiv, TLog, TExp, TMax, TMin, SizeOf,
      |    Empty,
      |    logic,
      |    Reg,
      |    RWire, Wire, BypassWire, PulseWire,
      |    RegFile,
      |    Vector,
      |    FIFO, FIFOF,
      |    Bits, Eq, Ord, Bounded,
      |    Arith, Literal, Bitwise, BitReduction, BitExtend,
      |    IsModule, TieOff, ToPut, ToGet,
      |    Add, Max, Log, FShow, Randomizable, RealLiteral,
      |    %% Attributes
      |    synthesize, noinline, doc, options,
      |    always_ready, always_enabled,
      |    ready, enable, result, prefix,
      |    fire_when_enabled, no_implicit_conditions,
      |    bit_blast, scan_insert,
      |    descending_urgency, preempts, conservative_implicit_conditions,
      |    internal_scheduling, method_scheduling,
      |    CLK, RST_N, RSTN, ungated_clock
      |    True,
      |    False,
      |    mkReg, mkRegU, mkRegA, mkRWire, mkWire, mkFIFO, mkSizedFIFOUG, mkFIFO1,
      |    mkBypassWire, mkDWire, mkPulseWire,
      |    pack, unpack, zeroExtend, signExtend, truncate, extend,
      |    fromInteger, inLiteralRange, negate,
      |    valueOf, valueof, SizeOf, defaultValue,
      |    minBound, maxBound,
      |    signedShiftRight, div, mod, exp, log2, add, abs, max, min, quot, rem,
      |    fromMaybe, isValid, validValue, noAction,
      |    error, warning, message, messageM,
      |    nosplit, emptyRules, addRules, rJoin, rJoinPreempts, rJoinDescendingUrgency,
      |    fshow, newVector,
      |    \$display,
      |    \$displayb,
      |    \$displayh,
      |    \$displayo,
      |    \$write,
      |    \$writeb,
      |    \$writeh,
      |    \$writeo,
      |    \$fopen,
      |    \$fclose,
      |    \$fgetc,
      |    \$ungetc,
      |    \$fflush,
      |    \$fdisplay,
      |    \$fdisplayb,
      |    \$fdisplayh,
      |    \$fdisplayo,
      |    \$fwrite,
      |    \$fwriteb,
      |    \$fwriteh,
      |    \$fwriteo,
      |    \$stop,
      |    \$finish,
      |    \$dumpon,
      |    \$dumpoff,
      |    \$dumpvars,
      |    \$dumpfile,
      |    \$dumpflush,
      |    \$time,
      |    \$stime,
      |    \$signed,
      |    \$unsigned,
      |    \$test\$plusargs,
      |    \$format
      |  },
      |  morecomment = [l]{//},
      |  morecomment = [s]{/*}{*/},
      |  morestring = [b]",
      |  morestring = [b]'
      |}
      |""".stripMargin


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

  def lstFormattingOfDocumentType(documentType: DocumentType.Value): LanguageFormatting = {
    require(listingStyle(documentType) == "language", "Listing style must be Language")
    val languageFormatting = documentType match {
      case DocumentType.Lando =>
        ReportTypes.LanguageFormatting("Lando",
          keywords = Array("system", "subsystem", "component", "relation", "contains", "inherit", "client", "events", "scenarios"),
          lineComment = "//",
          blockComment = BlockComment("/*", "*/"),
          literates = Array(Literate("—", ","), Literate("-", ","))
        )
      case DocumentType.Cryptol =>
        LanguageFormatting(
          "Cryptol",
          keywords = Array("type", "private", "import", "module", "where", "property", "if", "then", "else", "as", "take", "drop", "zero", "sum", "elem", "true", "false"),
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
          literates = Array(Literate("⊑", "$\\sqsubseteq$ "), Literate("use\\ case", "\\color{keywordcolor}\\bfseries use\\ case "), Literate("—", ","), Literate("⊢", "$\\vdash$ "), Literate("-", ","))
        )
      case DocumentType.SV =>
        LanguageFormatting(
          "SV",
          Array("module", "private", "import", "method", "input", "output", "assign", "endmodule", "end", "if", "else", "begin", "endfunction", "function", "endtask", "task", "for", "while", "repeat", "case", "endcase", "default", "return", "break", "continue", "rand", "randc", "randcase", "randsequence", "forever", "wait", "disable", "fork", "join", "join_any", "join_none", "void", "null", "super", "this", "const", "local", "static", "protected", "virtual", "automatic", "randsequence", "randcase", "randc", "rand", "typedef", "enum", "struct", "union", "class", "interface", "package", "program", "property", "sequence"),
          "//",
          BlockComment("/*", "*/"),
          literates = Array.empty[Literate]
        )
      case DocumentType.Lobot =>
        LanguageFormatting(
          "Lobot",
          Array("system", "subsystem", "with", "inherit", "client", "events", "types", "where", "type",
            "kind", "of", "int", "self", "struct", "with"),
          "--",
          BlockComment("/*", "*/"),
          literates = Array.empty[Literate]
        )
      case DocumentType.Saw =>
        val cryptolKeywords = lstFormattingOfDocumentType(DocumentType.Cryptol).keywords
        LanguageFormatting(
          "SAW",
          cryptolKeywords ++ Array("include", "let", "cryptol_add_path", "cryptol_load", "llvm_verify",
            "llvm_return", "llvm_execute_func", "z3", "llvm_load_module", "do", "ptr_to_fresh",
            "llvm_fresh_var", "llvm_term", "llvm_precond", "llvm_int", "write_verilog", "enable_experimental"),
          "//",
          BlockComment("/*", "*/"),
          literates = Array(Literate("/\\\\", "$\\land$ "), Literate("\\\\/", "$\\lor$ "))
        )
      case _ => throw new Exception("Unknown document type. There is no defined latex language for the type of Document: " + documentType)
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
    val formatting = lstFormattingOfDocumentType(DocumentType.Lando)
    buildLanguageFormatting(formatting)
  }

  lazy val lobotFormatting: String = {
    val formatting = lstFormattingOfDocumentType(DocumentType.Lobot)
    buildLanguageFormatting(formatting)
  }

  lazy val cryptolFormatting: String = {
    val formatting = lstFormattingOfDocumentType(DocumentType.Cryptol)
    buildLanguageFormatting(formatting)
  }

  lazy val sawFormatting: String = {
    """\lstdefinelanguage{saw}{
      |  basicstyle=\scriptsize\ttfamily,
      |  keywordstyle=\color{keywordcolor}\bfseries,
      |  commentstyle=\itshape,
      |  comment = [l]{//},
      |  morecomment  = [is]{/*}{*/},
      |  extendedchars=\true,
      |  language=Cryptol,
      |  morekeywords={include, let, cryptol\_add\_path, cryptol\_load, llvm\_verify,
      |  llvm\_return, llvm\_execute\_func, z3, llvm\_load\_module, false, true,
      |  do, ptr_to_fresh, llvm\_fresh\_var, llvm\_term, llvm\_precond, llvm\_int, write\_verilog,enable\_experimental}
      |}""".stripMargin
  }

  // C formatting
  lazy val cFormatting: String = {
    """\lstdefinelanguage{CStyle}{
      |  basicstyle=\scriptsize\ttfamily,
      |  keywordstyle=\color{keywordcolor}\bfseries,
      |  commentstyle=\itshape,
      |  extendedchars=\true,
      |  language=C,
      |  morekeywords={uint8_t, uint16_t, uint32_t, ssize_t, size_t, uint64_t, uint128_t, uint256_t, inline, malloc}
      |}""".stripMargin
  }

  //Json Formatting to be used to include FRET documents in the documentation
  lazy val jsonFormatting: String = {
    """
      |\colorlet{punct}{red!60!black}
      |\definecolor{delim}{RGB}{20,105,176}
      |\colorlet{numb}{magenta!60!black}
      |
      \lstdefinelanguage{json}{
      |    basicstyle=\scriptsize\ttfamily,
      |    keywordstyle=\color{keywordcolor}\bfseries,
      |    commentstyle=\itshape,
      |    numbers=left,
      |    numberstyle=\scriptsize,
      |    stepnumber=1,
      |    numbersep=8pt,
      |    showstringspaces=false,
      |    breaklines=true,
      |    frame=lines,
      |    literate=
      |     *{0}{{{\color{numb}0}}}{1}
      |      {1}{{{\color{numb}1}}}{1}
      |      {2}{{{\color{numb}2}}}{1}
      |      {3}{{{\color{numb}3}}}{1}
      |      {4}{{{\color{numb}4}}}{1}
      |      {5}{{{\color{numb}5}}}{1}
      |      {6}{{{\color{numb}6}}}{1}
      |      {7}{{{\color{numb}7}}}{1}
      |      {8}{{{\color{numb}8}}}{1}
      |      {9}{{{\color{numb}9}}}{1}
      |      {:}{{{\color{punct}{:}}}}{1}
      |      {,}{{{\color{punct}{,}}}}{1}
      |      {\{}{{{\color{delim}{\{}}}}{1}
      |      {\}}{{{\color{delim}{\}}}}}{1}
      |      {[}{{{\color{delim}{[}}}}{1}
      |      {]}{{{\color{delim}{]}}}}{1},
      |}""".stripMargin
  }


  lazy val sysmlFormatting: String = {
    val formatting = lstFormattingOfDocumentType(DocumentType.SysML)
    buildLanguageFormatting(formatting)
  }
}
