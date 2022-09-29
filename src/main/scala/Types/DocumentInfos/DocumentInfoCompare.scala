package Types.DocumentInfos

import Types.ReferenceType

object DocumentInfoCompare {
  def compare(document1: DocumentInfo, document2: DocumentInfo): Boolean = {
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

  //Comparision of References
}