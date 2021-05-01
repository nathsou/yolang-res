open Belt
open Types

type subst = Map.Int.t<monoTy>

let rec substMono = (s: subst, ty) =>
  switch ty {
  | TyVar(x) =>
    switch s->Map.Int.get(x) {
    | Some(t) => substMono(s, t)
    | None => TyVar(x)
    }
  | TyConst(name, args) => TyConst(name, args->Array.map(substMono(s)))
  }

let substPoly = (s: subst, (polyVars, ty): polyTy) => {
  (polyVars, substMono(s->Map.Int.removeMany(polyVars), ty))
}

let substCompose = (s1: subst, s2: subst): subst =>
  Map.Int.map(s2, substMono(s1))->Map.Int.merge(s1, (_, a, b) => Js.Option.firstSome(a, b))

let substComposeMany = (h, tl) => tl->Array.reduce(h, substCompose)

let substEnv = (s: subst, env: Types.Env.t) => {
  env->Map.String.map(substPoly(s))
}

let showSubst = (s: subst) =>
  "{\n" ++
  s
  ->Map.Int.toArray
  ->Array.joinWith("\n", ((k, v)) => `  ${showTyVar(k)} -> ${showMonoTy(v)}`) ++ "\n}"
