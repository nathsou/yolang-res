open Belt
open Types

type t = Map.Int.t<monoTy>

let empty = Map.Int.empty

let rec substMono = (s: t, ty: monoTy): monoTy =>
  switch ty {
  | TyVar(x) =>
    switch s->Map.Int.get(x) {
    | Some(t) => substMono(s, t)
    | None => TyVar(x)
    }
  | TyConst(name, args) => TyConst(name, args->Array.map(substMono(s)))
  | TyStruct(structTy) =>
    TyStruct(
      switch structTy {
      | NamedStruct(name) => NamedStruct(name)
      | PartialStruct(attrs) => PartialStruct(attrs->Map.String.map(substMono(s)))
      },
    )
  }

let substPoly = (s: t, (polyVars, ty): polyTy) => {
  (polyVars, substMono(s->Map.Int.removeMany(polyVars), ty))
}

let substCompose = (s1: t, s2: t): t =>
  Map.Int.map(s2, substMono(s1))->Map.Int.merge(s1, (_, a, b) => Js.Option.firstSome(b, a))

let substComposeMany = (h, tl) => tl->Array.reduce(h, substCompose)

let substEnv = (s: t, env: Types.Env.t) => {
  env->Map.String.map(substPoly(s))
}

let show = (s: t) =>
  "{" ++
  s->Map.Int.toArray->Array.joinWith(", ", ((k, v)) => `${showTyVar(k)} -> ${showMonoTy(v)}`) ++ "}"
