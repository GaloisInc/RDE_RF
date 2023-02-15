package Types.FRET

final case class FRETRequirement(
                                  reqid: String,
                                  parent_reqid: String,
                                  rationale: String,
                                  fulltext: String,
                                  semantics: FRETSemantics,
                                ) {
  require(reqid.nonEmpty, "reqid must not be empty")
  require(fulltext.nonEmpty, "fulltext must not be empty")
  require(semantics.description.nonEmpty, "semantics must not be empty")
}

final case class FRETVariable(
                               variable_name: String,
                               dataType: String,
                               idType: String,
                               completed: Boolean,
                               modeldoc: Boolean
                             ) {
  require(variable_name.nonEmpty, "variable_name must not be empty")
  require(dataType.nonEmpty, "dataType must not be empty")
  require(idType.nonEmpty, "idType must not be empty")
}


final case class FRETSemantics(description: String)