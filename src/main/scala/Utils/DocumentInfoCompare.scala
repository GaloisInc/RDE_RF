package Utils

import Types.DocumentInfos.DocumentInfo
import Types.ReferenceType


trait DocumentInfoCompare {
  def compare[T <: DocumentInfo[T]](document1: T, document2: T): Boolean

  def compareAddedReferences[T <: DocumentInfo[T]](document1: T, document2: T): Boolean

}

object DocumentInfoCompare extends DocumentInfoCompare {
  def compareAddedReferences[T <: DocumentInfo[T]](newDocument: T, document2: T): Boolean = {
    assert(newDocument.documentName == document2.documentName, "Document names must be equal but are " + newDocument.documentName + " and " + document2.documentName)
    assert(newDocument.documentType == document2.documentType, "Document types must be equal but are " + newDocument.documentType + " and " + document2.documentType)
    assert(newDocument.filePath == document2.filePath, "File paths must be equal but are " + newDocument.filePath + " and " + document2.filePath)
    assert(newDocument.getRelations.size == document2.getRelations.size, "Number of relations must be equal but are " + newDocument.getRelations.size + " and " + document2.getRelations.size)
    assert(newDocument.getAllReferences.size >= document2.getAllReferences.size, "Number of references must be equal or less but are " + newDocument.getAllReferences.size + " for " + newDocument.documentName +
      " and " + document2.getAllReferences.size + " for " + document2.documentName)
    true
  }

  def compare[T <: DocumentInfo[T]](document1: T, document2: T): Boolean = {
    assert(document1.documentName == document2.documentName, "Document names must be equal but are " + document1.documentName + " and " + document2.documentName)
    assert(document1.documentType == document2.documentType, "Document types must be equal but are " + document1.documentType + " and " + document2.documentType)
    assert(document1.filePath == document2.filePath, "File paths must be equal but are " + document1.filePath + " and " + document2.filePath)
    assert(document1.getRelations.size == document2.getRelations.size, "Number of relations must be equal but are " + document1.getRelations.size + " and " + document2.getRelations.size)
    assert(document1.getAllReferences.count(_.getReferenceType == ReferenceType.System) == document2.getAllReferences.count(_.getReferenceType == ReferenceType.System), "Number of system references must be equal but are " + document1.getAllReferences.count(_.getReferenceType == ReferenceType.System) + " and " + document2.getAllReferences.count(_.getReferenceType == ReferenceType.System))
    assert(document1.getAllReferences.count(_.getReferenceType == ReferenceType.SubSystem) == document2.getAllReferences.count(_.getReferenceType == ReferenceType.SubSystem), "Number of subsystem references must be equal but are " + document1.getAllReferences.count(_.getReferenceType == ReferenceType.SubSystem) + " and " + document2.getAllReferences.count(_.getReferenceType == ReferenceType.SubSystem))
    assert(document1.getAllReferences.count(_.getReferenceType == ReferenceType.Component) == document2.getAllReferences.count(_.getReferenceType == ReferenceType.Component), "Number of component references must be equal but are " + document1.getAllReferences.count(_.getReferenceType == ReferenceType.Component) + " and " + document2.getAllReferences.count(_.getReferenceType == ReferenceType.Component))
    document1.getAllReferences.count(_.getReferenceType == ReferenceType.Scenario) == document2.getAllReferences.count(_.getReferenceType == ReferenceType.Scenario) &&
      document1.getAllReferences.count(_.getReferenceType == ReferenceType.Requirement) == document2.getAllReferences.count(_.getReferenceType == ReferenceType.Requirement) &&
      document1.getAllReferences.count(_.getReferenceType == ReferenceType.Event) == document2.getAllReferences.count(_.getReferenceType == ReferenceType.Event) &&
      document1.getAllReferences.count(_.getReferenceType == ReferenceType.Connection) == document2.getAllReferences.count(_.getReferenceType == ReferenceType.Connection) &&
      document1.getAllReferences.count(_.getReferenceType == ReferenceType.Import) == document2.getAllReferences.count(_.getReferenceType == ReferenceType.Import) &&
      document1.getAllReferences.count(_.getReferenceType == ReferenceType.View) == document2.getAllReferences.count(_.getReferenceType == ReferenceType.View) &&
      document1.getAllReferences.count(_.getReferenceType == ReferenceType.ViewPoint) == document2.getAllReferences.count(_.getReferenceType == ReferenceType.ViewPoint) &&
      document1.getAllReferences.count(_.getReferenceType == ReferenceType.Type) == document2.getAllReferences.count(_.getReferenceType == ReferenceType.Type) &&
      document1.getAllReferences.count(_.getReferenceType == ReferenceType.Attribute) == document2.getAllReferences.count(_.getReferenceType == ReferenceType.Attribute)

  }
}