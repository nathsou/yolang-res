open Belt

type rec monoTy = TyVar(int) | TyConst(string, array<monoTy>) | TyStruct(structTy)
and structTy = NamedStruct(string) | PartialStruct(Map.String.t<monoTy>)

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

let rec freeTyVarsMonoTy = (ty: monoTy) => {
  open Set.Int
  switch ty {
  | TyVar(a) => empty->add(a)
  | TyConst(_, args) => args->Array.map(freeTyVarsMonoTy)->Array.reduce(empty, union)
  | TyStruct(structTy) =>
    switch structTy {
    | NamedStruct(_) => empty
    | PartialStruct(attrs) =>
      attrs->Map.String.valuesToArray->Array.map(freeTyVarsMonoTy)->Array.reduce(empty, union)
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
  | TyStruct(structTy) =>
    switch structTy {
    | NamedStruct(name) => name
    | PartialStruct(attrs) =>
      "{ " ++
      attrs
      ->Map.String.toArray
      ->Array.joinWith(", ", ((attr, ty)) => attr ++ ": " ++ ty->showMonoTy) ++ " }"
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
