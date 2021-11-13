open Belt

type attribute = {
  name: string,
  mut: bool,
  offset: int,
  ty: Types.monoTy,
  size: int,
  // implemented functions behave like attributes
  impl: option<(Name.nameRef, bool)>,
}

type staticFunc = {name: Name.nameRef}

type t = {name: string, attributes: array<attribute>, size: int, staticFuncs: array<staticFunc>}

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

  {name: name, attributes: attrs, size: offset.contents, staticFuncs: []}
}

let addImplementation = (self: t, name: Name.nameRef, isSelfMutable: bool) => {
  let _ = self.attributes->Js.Array2.push({
    name: name.contents.name,
    mut: false,
    ty: name.contents.ty,
    offset: 0,
    size: 0,
    impl: Some((name, isSelfMutable)),
  })
}

let addStaticFunc = (self: t, name: Name.nameRef) => {
  let _ = self.staticFuncs->Js.Array2.push({
    name: name,
  })
}

exception UndeclaredStruct(string)
exception StructTypeNotMatched(Types.monoTy)
exception AmibguousStruct(Types.Attributes.t, array<t>)
