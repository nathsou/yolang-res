open Belt

// TODO: traits
// http://smallcultfollowing.com/babysteps/blog/2017/01/26/lowering-rust-traits-to-logic/

type rec monoTy = TyVar(int) | TyConst(string, array<monoTy>) | TyStruct(structTy)
and structAttributes = StructTail(monoTy) | StructCons((string, monoTy), structAttributes)
and structTy = NamedStruct(string) | PartialStruct(structAttributes)

type polyTy = (array<int>, monoTy)

let polyOf = mono => ([], mono)

module Env = {
  open Map.String
  type t = Map.String.t<polyTy>

  let empty = Map.String.empty

  let add = (env: t, x, ty): t => env->set(x, ty)
  let addMono = (env: t, x, ty: monoTy): t => env->set(x, polyOf(ty))
  let remove = (env: t, x): t => env->remove(x)
  let has = (env: t, x): bool => env->has(x)
  let get = (env: t, x): option<polyTy> => env->get(x)

  let show = (env: t, showPolyTy: polyTy => string) => {
    let body =
      env
      ->Map.String.map(showPolyTy)
      ->Map.String.toArray
      ->Array.joinWith("\n", ((k, v)) => `  ${k}: ${v}`)

    `{\n${body}\n}`
  }
}

let u8Ty = TyConst("u8", [])
let u32Ty = TyConst("u32", [])
let charTy = TyConst("char", [])
let stringTy = TyConst("str", [])
let u64Ty = TyConst("u64", [])
let boolTy = TyConst("bool", [])
let unitTy = TyConst("()", [])

let funTy = (args, ret) => TyConst("Fun", Array.concat(args, [ret]))

let tupleTy = tys => TyConst("Tuple", tys)

let arrayTy = (elemTy: monoTy, len: int) => TyConst(
  "Array",
  [elemTy, TyConst(Int.toString(len), [])],
)

let pointerTy = ty => TyConst("Ptr", [ty])

let isReferencedTy = ty =>
  switch ty {
  | TyStruct(_) => true
  | TyConst("Array", _) => true
  | _ => false
  }

module Size = {
  exception UnkownTypeSize(monoTy)

  let rec size = (ty: monoTy) =>
    switch ty {
    | TyConst("()", []) => 0 // Zero-sized Type
    | TyConst("u8", []) => 1
    | TyConst("char", []) => 1
    | TyConst("bool", []) => 4
    | TyConst("u32", []) => 4
    | TyConst("Fun", _) => 4
    | TyConst("Ptr", _) => 4
    | TyStruct(_) => 4 // structs are references
    | TyConst("Array", [_elemTy, TyConst(_len, [])]) => 4 // arrays are references
    | TyConst("u64", []) => 8
    | TyConst("Mut", [ty]) => size(ty)
    | TyConst("Tuple", tys) => tys->Array.map(size)->Array.reduce(0, (p, c) => p + c)
    | _ => raise(UnkownTypeSize(ty))
    }

  let sizeLog2 = ty => Int.fromFloat(Js.Math.ceil_float(Js.Math.log2(Float.fromInt(size(ty)))))

  let isZeroSizedType = (ty: monoTy) => ty->size == 0
}

module Attributes = {
  type t = structAttributes

  let make = tail => StructTail(tail)

  // keep the attribute names sorted
  let rec insert = (self: t, attr, ty) => {
    switch self {
    | StructTail(a) => StructCons((attr, ty), StructTail(a))
    | StructCons((attr2, ty2), tail) =>
      if attr < attr2 {
        StructCons((attr, ty), StructCons((attr2, ty2), tail))
      } else {
        StructCons((attr2, ty2), tail->insert(attr, ty))
      }
    }
  }

  let rec tail = (self: t) =>
    switch self {
    | StructTail(a) => a
    | StructCons(_, tl) => tail(tl)
    }

  let fromArray = (attrs: array<(string, monoTy)>, tail) => {
    attrs->Array.reduce(make(TyVar(tail)), (acc, (attr, ty)) => acc->insert(attr, ty))
  }

  let toArray = (attrs: t) => {
    let rec aux = (attrs, acc) =>
      switch attrs {
      | StructCons(attr, tail)
      | StructTail(TyStruct(PartialStruct(StructCons(attr, tail)))) =>
        aux(tail, list{attr, ...acc})
      | StructTail(_) => acc
      }

    aux(attrs, list{})->List.toArray
  }

  let toMap = (attrs: t): Map.String.t<monoTy> => {
    Map.String.fromArray(toArray(attrs))
  }

  let rec map = (attrs, f) => {
    switch attrs {
    | StructTail(a) =>
      switch f(a) {
      | TyStruct(PartialStruct(t)) => t
      | t => StructTail(t)
      }
    | StructCons((attr, ty), tail) => StructCons((attr, f(ty)), map(tail, f))
    }
  }

  let show = (attrs: t, showMonoTy) => {
    let rec aux = (attr, acc) =>
      switch attr {
      | StructTail(a) => list{"..." ++ showMonoTy(a), ...acc}->List.toArray->Array.reverse
      | StructCons((attr, ty), tail) => aux(tail, list{attr ++ ": " ++ showMonoTy(ty), ...acc})
      }

    "{ " ++ aux(attrs, list{})->Array.joinWith(", ", x => x) ++ " }"
  }
}

let rec freeTyVarsMonoTy = (ty: monoTy) => {
  open Set.Int
  switch ty {
  | TyVar(a) => empty->add(a)
  | TyConst(_, args) => args->Array.map(freeTyVarsMonoTy)->Array.reduce(empty, union)
  | TyStruct(structTy) =>
    switch structTy {
    | NamedStruct(_) => empty
    | PartialStruct(attrs) =>
      attrs
      ->Attributes.toArray
      ->Array.map(snd->Utils.Func.compose(freeTyVarsMonoTy))
      ->Array.reduce(empty, union)
    }
  }
}

let freeTyVarsPolyTy = ((vars, ty): polyTy) =>
  freeTyVarsMonoTy(ty)->Set.Int.diff(Set.Int.fromArray(vars))

let freeTyVarsEnv = (env: Env.t) =>
  env
  ->Map.String.valuesToArray
  ->Array.map(freeTyVarsPolyTy)
  ->Array.reduce(Set.Int.empty, Set.Int.union)

let generalizeTy = (env: Env.t, ty: monoTy): polyTy => {
  let vars = freeTyVarsMonoTy(ty)->Set.Int.diff(freeTyVarsEnv(env))->Set.Int.toArray
  (vars, ty)
}

let showTyVar = n => {
  let name = Js.String.fromCharCode(97 + mod(n, 26))
  let k = n / 26

  if k == 0 {
    name
  } else {
    `${name}${Int.toString(k)}`
  }
}

let rec showMonoTy = ty =>
  switch ty {
  | TyVar(tau) => showTyVar(tau)
  | TyConst(name, []) => name
  | TyConst("Fun", args) => {
      let (args, ret) = (
        args->Array.slice(~offset=0, ~len=args->Array.length - 1),
        args->Array.getExn(args->Array.length - 1),
      )

      switch args {
      | [arg] => `${showMonoTy(arg)} -> ${showMonoTy(ret)}`
      | _ => `(${args->Array.joinWith(", ", showMonoTy)}) -> ${showMonoTy(ret)}`
      }
    }
  | TyConst("Tuple", args) => `(${args->Array.joinWith(", ", showMonoTy)})`
  | TyConst("Array", [elemTy, TyConst(len, [])]) => `[${showMonoTy(elemTy)}; ${len}]`
  | TyConst(name, args) => `${name}<${args->Array.joinWith(", ", showMonoTy)}>`
  | TyStruct(structTy) =>
    switch structTy {
    | NamedStruct(name) => name
    | PartialStruct(attrs) => attrs->Attributes.show(showMonoTy)
    }
  }

let showPolyTy = ((polyVars, ty): polyTy) => {
  if polyVars == [] {
    showMonoTy(ty)
  } else {
    `forall ${polyVars->Array.joinWith(", ", showTyVar)}. ${showMonoTy(ty)}`
  }
}
