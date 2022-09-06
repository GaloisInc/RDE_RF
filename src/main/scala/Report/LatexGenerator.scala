package Report

import Types.DocumentInfos.{CryptolDocumentInfo, LandoDocumentInfo, SysMLDocumentInfo}

object LatexGenerator {

  def includeListing(documentInfo: LandoDocumentInfo, listing: String, language: String): String = {
    //val listingName =  documentInfo.listingName(listing)
    s"""\\begin{lstlisting}[language=$language]}]
       |Name
       |\\end{lstlisting}
       |""".stripMargin
  }

  def generateLatexReport(report: ReportReference): String = {
    val latex = new StringBuilder()

    latex.toString()
  }

}

case class ReportReference(title: String,
                           landoDocuments: List[LandoDocumentInfo],
                           sysmlDocuments: List[SysMLDocumentInfo],
                           cryptolDocuments: List[CryptolDocumentInfo]
                          )
