open Belt
open Types

type t = Map.Int.t<monoTy>

let rec substMono = (s: t, ty) =>
  switch ty {
  | TyVar(x) =>
    switch s->Map.Int.get(x) {
    | Some(t) => substMono(s, t)
    | None => TyVar(x)
    }
  | TyConst(name, args) => TyConst(name, args->Array.map(substMono(s)))
  }

let substPoly = (s: t, (polyVars, ty): polyTy) => {
  (polyVars, substMono(s->Map.Int.removeMany(polyVars), ty))
}

let substCompose = (s1: t, s2: t): t =>
  Map.Int.map(s2, substMono(s1))->Map.Int.merge(s1, (_, a, b) => Js.Option.firstSome(a, b))

let substComposeMany = (h, tl) => tl->Array.reduce(h, substCompose)

let substEnv = (s: t, env: Types.Env.t) => {
  env->Map.String.map(substPoly(s))
}

let show = (s: t) =>
  "{\n" ++
  s
  ->Map.Int.toArray
  ->Array.joinWith("\n", ((k, v)) => `  ${showTyVar(k)} -> ${showMonoTy(v)}`) ++ "\n}"
