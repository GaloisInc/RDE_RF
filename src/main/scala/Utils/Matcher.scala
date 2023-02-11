package Utils

import Types.ReferenceName

/**
 * This object contains methods to match a name to a reference name.
 */
object Matcher {
  def referenceNameMatches(name: String, referenceName: ReferenceName): Boolean = {
    require(name.nonEmpty, "name must not be empty")
    val cleanName = name.stripPrefix("'").stripSuffix("'").trim()
    if (referenceName.acronym.isDefined) {
      cleanName.equalsIgnoreCase(referenceName.acronym.get) || cleanName.equalsIgnoreCase(referenceName.name)
    } else {
      cleanName.equalsIgnoreCase(referenceName.name)
    }
  }

  def getReferenceName(name: String, referenceName: ReferenceName): Option[String] = {
    require(name.nonEmpty, "name must not be empty")
    val cleanName = name.stripPrefix("'").stripSuffix("'").trim()
    if (cleanName.equalsIgnoreCase(referenceName.name)) return Some(referenceName.name)
    if (referenceName.acronym.isDefined && cleanName.equalsIgnoreCase(referenceName.acronym.get)) return Some(referenceName.acronym.get)
    None
  } ensuring(_.isDefined || !referenceNameMatches(name, referenceName), "if name matches referenceName, then getReferenceName must return Some")
}