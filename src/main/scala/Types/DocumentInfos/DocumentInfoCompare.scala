package Types.DocumentInfos

import Types.ReferenceType

object DocumentInfoCompare {
  def compare(document1: DocumentInfo, document2: DocumentInfo): Boolean = {
    document1.documentName == document2.documentName
      && document1.documentType == document2.documentType
      && document1.filePath == document2.filePath
      && document1.getRelations.size == document2.getRelations.size
      && document1.getAllReferences.size == document2.getAllReferences.size
      && document1.getAllReferences.count(_.referenceType == ReferenceType.System) == document2.getAllReferences.count(_.referenceType == ReferenceType.System)
      && document1.getAllReferences.count(_.referenceType == ReferenceType.SubSystem) == document2.getAllReferences.count(_.referenceType == ReferenceType.SubSystem)
      && document1.getAllReferences.count(_.referenceType == ReferenceType.Component) == document2.getAllReferences.count(_.referenceType == ReferenceType.Component)
      && document1.getAllReferences.count(_.referenceType == ReferenceType.Scenario) == document2.getAllReferences.count(_.referenceType == ReferenceType.Scenario)
      && document1.getAllReferences.count(_.referenceType == ReferenceType.Requirement) == document2.getAllReferences.count(_.referenceType == ReferenceType.Requirement)
      && document1.getAllReferences.count(_.referenceType == ReferenceType.Event) == document2.getAllReferences.count(_.referenceType == ReferenceType.Event)
      && document1.getAllReferences.count(_.referenceType == ReferenceType.Connection) == document2.getAllReferences.count(_.referenceType == ReferenceType.Connection)
      && document1.getAllReferences.count(_.referenceType == ReferenceType.Import) == document2.getAllReferences.count(_.referenceType == ReferenceType.Import)
      && document1.getAllReferences.count(_.referenceType == ReferenceType.View) == document2.getAllReferences.count(_.referenceType == ReferenceType.View)
      && document1.getAllReferences.count(_.referenceType == ReferenceType.ViewPoint) == document2.getAllReferences.count(_.referenceType == ReferenceType.ViewPoint)
      && document1.getAllReferences.count(_.referenceType == ReferenceType.Type) == document2.getAllReferences.count(_.referenceType == ReferenceType.Type)
      && document1.getAllReferences.count(_.referenceType == ReferenceType.Attribute) == document2.getAllReferences.count(_.referenceType == ReferenceType.Attribute)
  }
}