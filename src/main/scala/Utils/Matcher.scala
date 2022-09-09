package Utils

import Types.ReferenceName

object Matcher {
  def referenceNameMatches(name: String, referenceName: ReferenceName): Boolean = {
    if (referenceName.acronym.isDefined) {
      name.equalsIgnoreCase(referenceName.acronym.get)
    } else {
      name.equalsIgnoreCase(referenceName.name)
    }
  }
}