package Types.DocumentInfos

import Types.ReferenceType

object DocumentInfoCompare {
  def compare(document1: DocumentInfo, document2: DocumentInfo): Boolean = {
    document1.documentName == document2.documentName &&
      document1.documentType == document2.documentType &&
      document1.filePath == document2.filePath &&
      document1.getRelations.size == document2.getRelations.size &&
      document1.getAllReferences.size == document2.getAllReferences.size &&
      document1.getAllReferences.count(_.getReferenceType == ReferenceType.System) == document2.getAllReferences.count(_.getReferenceType == ReferenceType.System) &&
      document1.getAllReferences.count(_.getReferenceType == ReferenceType.SubSystem) == document2.getAllReferences.count(_.getReferenceType == ReferenceType.SubSystem) &&
      document1.getAllReferences.count(_.getReferenceType == ReferenceType.Component) == document2.getAllReferences.count(_.getReferenceType == ReferenceType.Component) &&
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