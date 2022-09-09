package Types

import org.scalactic.Equality
import org.scalactic.TypeCheckedTripleEquals.convertToCheckingEqualizer

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
  Attribute,
  Relation
}

enum RelationType {
  case client, inherit, contains, Link
}

enum LatexReferenceType {
  case
  File,
  Abstraction,
  Refinement,
  Link,
  CryptolProperty,
  ConnectionArtifact
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