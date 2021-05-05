open Belt

type name = {
  mutable name: string,
  index: int,
  mutable ty: Types.monoTy,
}

type nameRef = ref<name>
type renameMap = HashMap.Int.t<nameRef>

type t = {mutable tyVarIndex: int, identifiers: array<string>, renaming: renameMap}

let make = (): t => {
  tyVarIndex: 0,
  identifiers: [],
  renaming: HashMap.Int.make(~hintSize=10),
}

// global context
let context = make()

let freshTyVar = () => {
  let res = Types.TyVar(context.tyVarIndex)
  context.tyVarIndex = context.tyVarIndex + 1
  res
}

/**
 * creates a fresh monomorphic instance of a polymorphic type
 */
let freshInstance = ((polyVars, ty): Types.polyTy): Types.monoTy => {
  let freshTyVars = polyVars->Array.map(_ => freshTyVar())
  Subst.substMono(Array.zip(polyVars, freshTyVars)->Map.Int.fromArray, ty)
}

let freshIdentifier = (name: string): nameRef => {
  let index = context.identifiers->Array.length
  let _ = context.identifiers->Js.Array2.push(name)

  let nameRef = ref({
    name: name,
    index: index,
    ty: freshTyVar(),
  })

  context.renaming->HashMap.Int.set(index, nameRef)

  nameRef
}

let getIdentifier = (name: string): nameRef => {
  switch context.identifiers
  ->ArrayUtils.getReverseIndexBy(n => n == name)
  ->Option.flatMap(index => context.renaming->HashMap.Int.get(index)) {
  | Some(id) => id
  | None => Js.Exn.raiseError(`unbound identifier "${name}"`)
  }
}

let substIdentifiers = (s: Subst.t): unit => {
  context.renaming
  ->HashMap.Int.valuesToArray
  ->Array.forEach(id => {
    id.contents.ty = s->Subst.substMono(id.contents.ty)
  })
}
