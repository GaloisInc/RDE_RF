package Specs

import Report.ReportTypes.ReportReference
import Types.DocumentInfos.DocumentInfo
import Utils.FileUtil

object FileSpecs {
  def allFilesExist(filesToAnalyze: Set[String]): Boolean = {
    filesToAnalyze.forall(file => {
      assert(FileUtil.fileExists(file), s"File $file does not exist")
      true
    })
  }

  private def allFilesOfCorrectType(filesToAnalyze: Set[String], supportedTypes: Set[String]): Boolean = {
    filesToAnalyze.forall(file => {
      assert(supportedTypes.exists(file.endsWith), s"File $file has unsupported type")
      true
    })
  }


  def fileChecks(filesToAnalyze: Set[String], supportedTypes: Set[String]): Boolean = {
    allFilesExist(filesToAnalyze) && allFilesOfCorrectType(filesToAnalyze, supportedTypes)
  }

  def allFilesAnalyzed[T <: DocumentInfo[T]](filesToAnalyze: Set[String], reportReference: ReportReference): Boolean = {
    filesAnalyzed(filesToAnalyze, reportReference.bsvDocuments, Types.DocumentType.BSV)
    filesAnalyzed(filesToAnalyze, reportReference.cryptolDocuments, Types.DocumentType.Cryptol)
    filesAnalyzed(filesToAnalyze, reportReference.landoDocuments, Types.DocumentType.Lando)
    filesAnalyzed(filesToAnalyze, reportReference.svDocuments, Types.DocumentType.SV)
    filesAnalyzed(filesToAnalyze, reportReference.sysmlDocuments, Types.DocumentType.SysML)
  }

  def allFilesAnalyzed[T <: DocumentInfo[T]](filesToAnalyze: Set[String], documents: Array[T]): Boolean = {
    val allDocuments = documents.map(_.filePath).toSet
    assert(allFilesExist(allDocuments), "Not all of the analyzed files exist")
    filesAnalyzed(filesToAnalyze, documents, Types.DocumentType.BSV)
    filesAnalyzed(filesToAnalyze, documents, Types.DocumentType.Cryptol)
    filesAnalyzed(filesToAnalyze, documents, Types.DocumentType.Lando)
    filesAnalyzed(filesToAnalyze, documents, Types.DocumentType.SV)
    filesAnalyzed(filesToAnalyze, documents, Types.DocumentType.SysML)
    true
  }

  private def filesAnalyzed[T <: DocumentInfo[T]](filesToAnalyze: Set[String], documents: Array[T], documentType: Types.DocumentType.Value): Boolean = {
    val extension = documentType.toString
    val files = documents.filter(_.filePath.endsWith(extension))
    val filesAnalyzedOfType = filesToAnalyze.filter(_.endsWith(extension))

    files.forall(doc => {
      assert(doc.filePath.endsWith(extension), s"File ${doc.filePath} is not a $extension file")
      assert(filesToAnalyze.contains(doc.filePath), s"File ${doc.filePath} was not analyzed")
      true
    })

    filesAnalyzedOfType.forall(file => {
      assert(files.exists(_.filePath == file), s"File $file was not analyzed." +
        s" Files analyzed: ${files.map(_.filePath).mkString(", ")}")
      true
    })
    true
  }


}
