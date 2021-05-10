open Belt

type rec monoTy = TyVar(int) | TyConst(string, array<monoTy>)

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
}

let u32Ty = TyConst("u32", [])
let u64Ty = TyConst("u64", [])
let boolTy = TyConst("bool", [])
let unitTy = TyConst("()", [])

let funTy = (args, ret) => TyConst("Fun", Array.concat(args, [ret]))

let tupleTy = tys => TyConst("Tuple", tys)

let pointerTy = ty => TyConst("Ptr", [ty])

exception UnkownTypeSize(monoTy)

let rec sizeLog2 = (ty: monoTy) =>
  switch ty {
  | TyConst("u32", []) => 2
  | TyConst("u64", []) => 3
  | TyConst("bool", []) => 2
  | TyConst("()", []) => 0 // Zero-sized Type
  | TyConst("Fun", _) => 2
  | TyConst("Ptr", _) => 2
  | TyConst("Tuple", tys) => tys->Array.map(sizeLog2)->Array.reduce(0, (p, c) => p + c)
  | _ => raise(UnkownTypeSize(ty))
  }

let isZeroSizedType = (ty: monoTy) => ty->sizeLog2 == 0

let rec freeTyVarsMonoTy = (ty: monoTy) => {
  open Set.Int
  switch ty {
  | TyVar(a) => empty->add(a)
  | TyConst(_, args) => args->Array.map(freeTyVarsMonoTy)->Array.reduce(empty, union)
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
  | TyConst(name, args) =>
    switch args {
    | [] => name
    | _ =>
      switch name {
      | "Fun" => {
          let (args, ret) = (
            args->Array.slice(~offset=0, ~len=args->Array.length - 1),
            args->Array.getExn(args->Array.length - 1),
          )

          switch args {
          | [arg] => `${showMonoTy(arg)} -> ${showMonoTy(ret)}`
          | _ => `(${args->Array.joinWith(", ", showMonoTy)}) -> ${showMonoTy(ret)}`
          }
        }
      | "Tuple" => `(${args->Array.joinWith(", ", showMonoTy)})`
      | _ => `${name}<${args->Array.joinWith(", ", showMonoTy)}>`
      }
    }
  }

let showPolyTy = ((polyVars, ty): polyTy) => {
  if polyVars == [] {
    showMonoTy(ty)
  } else {
    `forall ${polyVars->Array.joinWith(", ", showTyVar)}. ${showMonoTy(ty)}`
  }
}

let showTyEnv = env => {
  let body =
    env
    ->Map.String.map(showPolyTy)
    ->Map.String.toArray
    ->Array.joinWith("\n", ((k, v)) => `  ${k}: ${v}`)

  `{\n${body}\n}`
}
