open Belt
open Types
open Context

type t = NoMatch | OneMatch(Struct.t) | MultipleMatches(array<Struct.t>)

let ambiguousMatchesError = (attributes: Attributes.t, matches: array<Struct.t>) => {
  "cannot infer struct type: inferred type " ++
  attributes->Attributes.show(showMonoTy) ++
  " matches " ++
  matches->Array.joinWith(", ", ({name}) => name)
}

// matches a PartialStruct with declared structures
let findMatchingStruct = (attributes: Attributes.t): t => {
  let matches =
    context.structs
    ->HashMap.String.valuesToArray
    ->Array.keepMap(struct => {
      Unification.unify(
        TyStruct(PartialStruct(attributes)),
        struct->Struct.toPartialStructType(freshTyVarIndex()),
      )->Result.mapWithDefault(None, _ => Some(struct))
    })

  switch matches {
  | [] => NoMatch
  | [s] => OneMatch(s)
  | _ => MultipleMatches(matches)
  }
}
