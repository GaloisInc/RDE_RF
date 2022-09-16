package Utils

import Types.ReferenceName

object Matcher {
  def referenceNameMatches(name: String, referenceName: ReferenceName): Boolean = {
    val cleanName = name.stripPrefix("'").stripSuffix("'").strip()
    if (referenceName.acronym.isDefined) {
      cleanName.equalsIgnoreCase(referenceName.acronym.get) || cleanName.equalsIgnoreCase(referenceName.name)
    } else {
      cleanName.equalsIgnoreCase(referenceName.name)
    }
  }

  def getReferenceName(name: String, referenceName: ReferenceName): Option[String] = {
    val cleanName = name.stripPrefix("'").stripSuffix("'").strip()
    if (cleanName.equalsIgnoreCase(referenceName.name)) return Some(referenceName.name)
    if (referenceName.acronym.isDefined && cleanName.equalsIgnoreCase(referenceName.acronym.get)) return Some(referenceName.acronym.get)
    None
  }
}