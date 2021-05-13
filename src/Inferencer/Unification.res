open Belt
open Types

let rec occurs = (x, ty) =>
  switch ty {
  | TyVar(y) => x == y
  | TyConst(_, args) => args->Array.some(occurs(x))
  | TyStruct(structTy) =>
    switch structTy {
    | NamedStruct(_) => false
    | PartialStruct(attrs) => attrs->Map.String.some((_, ty) => x->occurs(ty))
    }
  }

let bindVar = (x, ty, subst): result<Subst.t, string> => {
  if x->occurs(ty) {
    Error(`${Types.showMonoTy(TyVar(x))} occurs in ${Types.showMonoTy(ty)}`)
  } else {
    switch subst->Map.Int.get(x) {
    | Some(t) =>
      if t == ty {
        Ok(subst)
      } else {
        Error(`${showMonoTy(ty)} is not unifiable with ${showMonoTy(t)}`)
      }
    | None => Ok(subst->Map.Int.set(x, ty))
    }
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
    | (TyStruct(a), TyStruct(b)) =>
      switch (a, b) {
      | (NamedStruct(a), NamedStruct(b)) if a == b => unifyMany(eqs, subst)
      | (PartialStruct(attrsA), PartialStruct(attrsB)) =>
        attrsA
        ->Map.String.toArray
        ->Array.map(((attr, tyA)) => {
          switch attrsB->Map.String.get(attr) {
          | Some(tyB) => Ok((tyA, tyB))
          | None =>
            Error(
              `struct type ${showMonoTy(t)} is missing attribute "${attr}" from type "${showMonoTy(
                  s,
                )}"`,
            )
          }
        })
        ->ArrayUtils.mapResult(x => x)
        ->Result.flatMap(newEqs => {
          unifyMany(newEqs->List.fromArray->List.concat(eqs), subst)
        })
      | (NamedStruct(a), PartialStruct(attrs)) =>
        switch Context.getStruct(a) {
        | Some(s) =>
          switch unifyMany(
            list{(TyStruct(PartialStruct(attrs)), s->Context.Struct.toPartialStructType), ...eqs},
            subst,
          ) {
          | Ok(subst) => Ok(subst)
          | Error(_) => Error(`struct type ${showMonoTy(t)} does not match ${a}`)
          }
        | None => Error(`undeclared struct "${a}"`)
        }
      | (PartialStruct(attrs), NamedStruct(b)) =>
        // order matters: {min: u32, max: u32} matches {min: u32} but not the other way around
        switch Context.getStruct(b) {
        | Some(struct) =>
          switch unifyMany(
            list{
              (TyStruct(PartialStruct(attrs)), struct->Context.Struct.toPartialStructType),
              ...eqs,
            },
            subst,
          ) {
          | Ok(subst) => Ok(subst)
          | Error(_) => Error(`struct type ${showMonoTy(s)} does not match ${b}`)
          }
        | None => Error(`undeclared struct "${b}"`)
        }
      | _ => Error(`cannot unify ${showMonoTy(s)} with ${showMonoTy(t)}`)
      }

    | (TyVar(x), TyVar(y)) if x == y => unifyMany(eqs, subst)
    | (TyVar(x), t) => bindVar(x, t, subst)->Result.flatMap(unifyMany(eqs))
    | (t, TyVar(x)) => bindVar(x, t, subst)->Result.flatMap(unifyMany(eqs))
    | _ => Error(`${showMonoTy(s)} is not unifiable with ${showMonoTy(t)}`)
    }
  }
}

let unify = (s, t) => unifyMany(list{(s, t)}, Map.Int.empty)
