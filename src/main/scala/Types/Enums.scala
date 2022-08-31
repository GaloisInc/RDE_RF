package Types

import org.scalactic.Equality
import org.scalactic.TypeCheckedTripleEquals.convertToCheckingEqualizer

enum LandoReferenceType {
  case Component, SubSystem, System, Scenario, Requirement, Event
}

enum SysMLType {
  case Action, Package, UseCase, Part, Requirement, Connection, Import, View, ViewPoint
}

enum ReferenceType {
  case
  Component,
  SubSystem,
  System,
  Scenario,
  Requirement,
  Event,
  Connection,
  Import,
  View,
  ViewPoint,
  Type,
  Attribute
}

enum RelationType {
  case client, inherit, contains, Link
}

enum DocumentType {
  case Lando, Lobot, SysML, Cryptol, Saw, SV, BSV
}

enum LandoLineType {
  case EmptyLine, Comment, Requirement, Event, Scenario, Reference, Relation, LineToBeSkipped
}

enum FileType {
  case RequirementFile, ScenarioFile, EventFile, ComponentFile, ViewFile
}