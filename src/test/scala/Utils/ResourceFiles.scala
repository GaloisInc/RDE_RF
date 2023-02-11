package Utils

object ResourceFiles {
  private val systemVerilogFolder = "../SystemVerilog"
  private val bsvFolder = "../BSV"
  private val fretFolder = "../FRET"
  private val cryptolFolder = "../cryptol"
  private val landoFolder = "../Lando"
  private val sysmlFolder = "../SysML"
  private val sawFolder = "../Saw"
  private val lobotFolder = "../lobot"

  private def getResourcePath(path: String): String = {
    val url = getClass.getResource(path)
    url.getPath
  }

  def getFilesOfType(t: Types.DocumentType.documentType): Set[String] = {
    val path = t match {
      case Types.DocumentType.Lando => getResourcePath(landoFolder)
      case Types.DocumentType.Lobot => getResourcePath(lobotFolder)
      case Types.DocumentType.SysML => getResourcePath(sysmlFolder)
      case Types.DocumentType.Cryptol => getResourcePath(cryptolFolder)
      case Types.DocumentType.Saw => getResourcePath(sawFolder)
      case Types.DocumentType.SV => getResourcePath(systemVerilogFolder)
      case Types.DocumentType.BSV => getResourcePath(bsvFolder)
      case Types.DocumentType.Fret => getResourcePath(fretFolder)
      //TODO: This is a temporary solution as C is not yet supported
    }
    FileUtil.getFilesInDirectory(path)
  }

  def getFilesOfTypes(types: Set[Types.DocumentType.documentType]): Set[String] = {
    types.flatMap(getFilesOfType)
  }

  //TODO: This is a temporary solution, we should not hardcode the types here
  def getAllFiles: Set[String] = {
    val allFiles = getFilesOfTypes(Set(Types.DocumentType.Lando, Types.DocumentType.Lobot, Types.DocumentType.SysML, Types.DocumentType.Cryptol, Types.DocumentType.Saw, Types.DocumentType.SV, Types.DocumentType.BSV, Types.DocumentType.Fret))
    allFiles
  }
}