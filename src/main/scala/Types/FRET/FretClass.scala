package Types.FRET

final case class FRETRequirement(
                                  reqid: String,
                                  parent_reqid: String,
                                  rationale: String,
                                  fulltext: String,
                                  semantics: FRETSemantics,
                                ) {
  require(reqid.nonEmpty, "reqid must not be empty")
  require(parent_reqid.nonEmpty, "parent_reqid must not be empty")
  require(rationale.nonEmpty, "rationale must not be empty")
  require(fulltext.nonEmpty, "fulltext must not be empty")
  require(semantics.description.nonEmpty, "semantics must not be empty")
}


final case class FRETSemantics(description: String)