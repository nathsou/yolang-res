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
  type attribute = {name: string, offset: int, ty: Types.monoTy}
  type t = {name: string, attributes: array<attribute>, size: int}

  let show = ({name, attributes}: t) => {
    "struct " ++
    name ++
    "{\n" ++
    attributes->Array.joinWith(",\n", ({name: attrName, ty}) =>
      `  ${attrName}: ${Types.showMonoTy(ty)}`
    ) ++ "\n}"
  }
}

module Size = {
  exception UnkownTypeSize(Types.monoTy)

  let size = (ty: Types.monoTy, structs: HashMap.String.t<Struct.t>) =>
    switch ty {
    | TyConst("u32", []) => 4
    | TyConst("u64", []) => 8
    | TyConst("bool", []) => 4
    | TyConst("()", []) => 0 // Zero-sized Type
    | TyConst("Fun", _) => 4
    | TyConst("Ptr", _) => 4
    // | TyConst("Tuple", tys) => tys->Array.map(size)->Array.reduce(0, (p, c) => p + c)
    | TyConst(name, _) =>
      switch structs->HashMap.String.get(name) {
      | Some({size}) => size
      | None => raise(UnkownTypeSize(ty))
      }
    | _ => raise(UnkownTypeSize(ty))
    }

  let sizeLog2 = (ty, structs) =>
    Int.fromFloat(Js.Math.ceil_float(Js.Math.log2(Float.fromInt(size(ty, structs)))))

  let isZeroSizedType = (ty: Types.monoTy, structs) => ty->size(structs) == 0
}

let makeStruct = (name, attributes: array<(string, Types.monoTy)>, structs): Struct.t => {
  let offset = ref(0)
  let attrs: array<Struct.attribute> = []

  attributes->Array.forEach(((attr, ty)) => {
    let _ = attrs->Js.Array2.push({name: attr, ty: ty, offset: offset.contents})
    offset := offset.contents + ty->Size.size(structs)
  })

  {name: name, attributes: attrs, size: offset.contents}
}

type t = {
  mutable tyVarIndex: int,
  identifiers: array<string>,
  renaming: renameMap,
  structs: HashMap.String.t<Struct.t>,
}

let make = (): t => {
  tyVarIndex: 0,
  identifiers: [],
  renaming: HashMap.Int.make(~hintSize=10),
  structs: HashMap.String.make(~hintSize=5),
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

let substIdentifiers = (s: Subst.t): unit => {
  context.renaming
  ->HashMap.Int.valuesToArray
  ->Array.forEach(id => {
    id.contents.ty = s->Subst.substMono(id.contents.ty)
  })
}

let declareStruct = (s: Struct.t): unit => {
  context.structs->HashMap.String.set(s.name, s)
}

let getStruct = (name: string): option<Struct.t> => {
  context.structs->HashMap.String.get(name)
}
