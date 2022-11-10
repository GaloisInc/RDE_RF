package Types.FRET

case class FRETRequirement(
                          reqid: String,
                          parent_reqid: String,
                          rationale: String,
                          fulltext: String,
                          semantics: FRETSemantics,
                        )
case class FRETSemantics(
                        description: String
                    )