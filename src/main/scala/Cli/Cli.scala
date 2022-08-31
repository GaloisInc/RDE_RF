package Cli

import scala.io.StdIn
import scala.util.matching.Regex
import DocumentEnrichers.DocumentAnalyzer
import Utils.FileUtil

class Cli {

  // Makes sure our user CLI inputs are valid
  private val commandArgPattern: Regex = "(\\w+)\\s*(.*)".r

  private val fileUtil = FileUtil()

  // Introduction to and explanation of the application
  def intro(): Unit = println(
    """
      |#############################################################################################################################################
      |
      |You are interacting with a list of highly rated yet lesser known films.
      |The purpose of this application is for you to discover quality movies that you otherwise may have never heard of.
      |If you want more information on the movies such as the director and review scores, there is a command for that, too.
      |You can choose to look at the entire list or to filter it by genre. You can even add your own movies that I may have missed!""".stripMargin
  )

  // Ways users can sort through movies
  def cliOptions(): Unit = {
    println(
      """
        |#############################################################################################################################################
        |
        |Enrich your documentation using these options:
        |------------------------------------------------
        |Enrich [folder] --- (folder with files to enrich)
        |EnrichMove files and moves files to: [srcfolder, desFolder]
        |------------------------------------------------
        |Input "quit" or "exit" to leave the application.
        |------------------------------------------------""".stripMargin
    )

    print(">> ")
  }

  def menu(): Unit = {

    intro()
    var runMenu = true

    while (runMenu) {

      cliOptions()

      val input = StdIn.readLine()

      input match {
        case commandArgPattern(cmd, _) if cmd.equalsIgnoreCase("quit") || cmd.equalsIgnoreCase("exit") => runMenu = false
        case commandArgPattern(cmd, arg) if cmd.equalsIgnoreCase("enrich") => enrichFilesInFolder()
        case commandArgPattern(cmd, arg) if cmd.equalsIgnoreCase("enrichMove") => enrichFilesInFolderAndMoveTo()
        case commandArgPattern(cmd, arg) => println(s"""unrecognized command: "$cmd" or: "$arg". Please follow menu instructions exactly.""")
        case _ => println("Please insert a command consistent with the menu options.")
      }
    }

    println("Enjoy your Documentation!")
  }

  def enrichFilesInFolder(): Unit = {
    println("------------------------------------------------")

    println("Enter source folder:")
    val folder = StdIn.readLine()

    val filesToAnalyze = fileUtil.getListOfFiles(folder).toArray

    val enrichedFiles = DocumentAnalyzer.enrichAndSortFiles(filesToAnalyze)

    println("The documents are now enriched with Latex references.")
  }

  def enrichFilesInFolderAndMoveTo(): Unit = {
    println("------------------------------------------------")

    println("Enter source folder:")
    val sourceFolder = StdIn.readLine()

    println("Enter destination folder:")
    val destinationFolder = StdIn.readLine()

  }

}