package Types

object ReferenceType extends Enumeration {
  type referenceType = Value
  val Component,
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
  Relation = Value
}

object RelationTypes extends Enumeration {
  type relationType = Value
  val client, inherit, contains, Link = Value
}

object LatexReferenceTypes extends Enumeration {
  type latexReferenceType = Value
  val
  File,
  Abstraction,
  Refinement,
  Link,
  CryptolProperty,
  ConnectionArtifact = Value
}

object DocumentType extends Enumeration {
  type documentType = Value
  val Lando, Lobot, SysML, Cryptol, Saw, SV, BSV, C, Fret = Value

  override def toString(): String = {
    this match {
      case Lando => "lando"
      case Lobot => "lobot"
      case SysML => "sysml"
      case Cryptol => "cry"
      case Saw => "saw"
      case SV => "sv"
      case BSV => "bsv"
      case C => "c"
      case Fret => "json"
      case _ => throw new Exception("Unknown document type")
    }
  }
}

object LandoLineType extends Enumeration {
  type landoLineType = Value
  val EmptyLine, Comment, Requirement, Event, Scenario, Reference, Relation, LineToBeSkipped = Value
}

object FileType extends Enumeration {
  type fileType = Value
  val RequirementFile, ScenarioFile, EventFile, ComponentFile, ViewFile = Value
}