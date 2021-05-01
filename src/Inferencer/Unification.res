open Belt
open Subst
open Types

let rec occurs = (x, ty) =>
  switch ty {
  | TyVar(y) => x == y
  | TyConst(_, args) => args->Array.some(occurs(x))
  }

let bindVar = (x, ty, subst): result<subst, string> => {
  if x->occurs(ty) {
    Error(`${Types.showMonoTy(TyVar(x))} occurs in ${Types.showMonoTy(ty)}`)
  } else {
    Ok(subst->Map.Int.set(x, ty))
  }
}

let rec unifyMany = (eqs: list<(monoTy, monoTy)>, subst) => {
  open Array
  switch eqs {
  | list{} => Ok(subst)
  | list{(s, t), ...eqs} =>
    switch (s, t) {
    | (TyConst(f, fArgs), TyConst(g, gArgs)) if f == g && fArgs->length == gArgs->length => {
        let newEqs = zip(fArgs, gArgs)->reduce(eqs, (acc, (a, b)) => list{(a, b), ...acc})
        unifyMany(newEqs, subst)
      }
    | (TyVar(x), TyVar(y)) if x == y => unifyMany(eqs, subst)
    | (TyVar(x), t) => bindVar(x, t, subst)->Result.flatMap(unifyMany(eqs))
    | (t, TyVar(x)) => bindVar(x, t, subst)->Result.flatMap(unifyMany(eqs))
    | _ => Error(`${showMonoTy(s)} is not unifiable with ${showMonoTy(t)}`)
    }
  }
}

let unify = (s, t) => unifyMany(list{(s, t)}, Map.Int.empty)
