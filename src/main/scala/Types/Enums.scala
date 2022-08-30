package Types

enum LandoReferenceType {
  case Component, SubSystem, System, Scenario, Requirement, Event
}

enum SysMLType {
  case Action, Package, UseCase, Part, Requirement, Connection, Import, View, ViewPoint
}

enum ReferenceType {
  case Component, SubSystem, System, Scenario, Requirement, Event, Action, Package, UseCase, Part, Connection, Import, View, ViewPoint, Item, Type
}

enum RelationType {
  case client, inherit, contains, Link
}

enum DocumentType {
  case Lando, Lobot, SysML, HDL, Cryptol, Saw
}

enum LandoLineType {
  case EmptyLine, Comment, Requirement, Event, Scenario, Reference, Relation, LineToBeSkipped
}

enum SysMLLineType {
  case EmptyLine, Comment, Requirement, UseCase, Action, Package, Part
}

enum FileType {
  case RequirementFile, ScenarioFile, EventFile, ComponentFile, ViewFile
}