open Belt

type name = {
  name: string,
  mutable newName: string,
  index: int,
  mutable ty: Types.monoTy,
}

type nameRef = ref<name>
type renameMap = HashMap.Int.t<nameRef>

module Struct = {
  type attribute = {
    name: string,
    mut: bool,
    offset: int,
    ty: Types.monoTy,
    size: int,
    impl: option<(nameRef, bool)>,
  }

  type t = {name: string, attributes: array<attribute>, size: int}

  let show = ({name, attributes}: t) => {
    "struct " ++
    name ++
    "{\n" ++
    attributes->Array.joinWith(",\n", ({name: attrName, ty}) =>
      `  ${attrName}: ${Types.showMonoTy(ty)}`
    ) ++ "\n}"
  }

  let toPartialStructType = ({attributes}: t, tail: int) => {
    Types.TyStruct(
      PartialStruct(
        attributes->Array.map(({name, ty}) => (name, ty))->Types.Attributes.fromArray(tail),
      ),
    )
  }

  let make = (name, attributes: array<(string, Types.monoTy, bool)>): t => {
    let offset = ref(0)
    let attrs: array<attribute> = []

    attributes->Array.forEach(((attr, ty, mut)) => {
      let size = ty->Types.Size.size
      let _ = attrs->Js.Array2.push({
        name: attr,
        mut: mut,
        ty: ty,
        offset: offset.contents,
        size: size,
        impl: None,
      })
      offset := offset.contents + size
    })

    {name: name, attributes: attrs, size: offset.contents}
  }

  let addImpl = (self: t, name: nameRef, isSelfMutable: bool) => {
    let _ = self.attributes->Js.Array2.push({
      name: name.contents.name,
      mut: false,
      ty: name.contents.ty,
      offset: 0,
      size: 0,
      impl: Some((name, isSelfMutable)),
    })
  }
}

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

let getIdentifier = (name: string): nameRef => {
  switch context.identifiers
  ->ArrayUtils.getReverseIndexBy(n => n == name)
  ->Option.flatMap(index => context.renaming->HashMap.Int.get(index)) {
  | Some(id) => id
  | None => Js.Exn.raiseError(`unbound identifier "${name}"`)
  }
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
