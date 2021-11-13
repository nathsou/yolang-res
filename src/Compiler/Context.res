open Belt
open Name

type t = {
  mutable tyVarIndex: int,
  mutable identifiers: array<string>,
  renaming: renameMap,
  structs: HashMap.String.t<Struct.t>,
}

// global context
let context = {
  tyVarIndex: 0,
  identifiers: [],
  renaming: HashMap.Int.make(~hintSize=10),
  structs: HashMap.String.make(~hintSize=5),
}

let clear = (): unit => {
  context.tyVarIndex = 0
  context.identifiers = []
  context.renaming->HashMap.Int.clear
  context.structs->HashMap.String.clear
}

let freshTyVarIndex = () => {
  let res = context.tyVarIndex
  context.tyVarIndex = context.tyVarIndex + 1
  res
}

let freshTyVar = () => {
  Types.TyVar(freshTyVarIndex())
}

/**
 * creates a fresh monomorphic instance of a polymorphic type
 */
let freshInstance = ((polyVars, ty): Types.polyTy): Types.monoTy => {
  let freshTyVars = polyVars->Array.map(_ => freshTyVar())
  Subst.substMono(Array.zip(polyVars, freshTyVars)->Map.Int.fromArray, ty)
}

let freshIdentifier = (~ty=None, name: string): nameRef => {
  let index = context.identifiers->Array.length
  let _ = context.identifiers->Js.Array2.push(name)

  let nameRef = ref({
    name: name,
    newName: name,
    index: index,
    ty: switch ty {
    | Some(ty) => ty
    | None => freshTyVar()
    },
  })

  context.renaming->HashMap.Int.set(index, nameRef)

  nameRef
}

let getIdentifier = (name: string): option<nameRef> => {
  context.identifiers
  ->Utils.Array.getReverseIndexBy(n => n == name)
  ->Option.flatMap(index => context.renaming->HashMap.Int.get(index))
}

let substNameRef = (s: Subst.t, n: nameRef): unit => {
  n.contents.ty = s->Subst.substMono(n.contents.ty)
}

let substIdentifiers = (s: Subst.t): unit => {
  context.renaming->HashMap.Int.valuesToArray->Array.forEach(substNameRef(s))
}

let declareStruct = (s: Struct.t): unit => {
  context.structs->HashMap.String.set(s.name, s)
}

let getStruct = (name: string): option<Struct.t> => {
  context.structs->HashMap.String.get(name)
}
