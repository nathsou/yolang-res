open Belt

type t = {mutable tyVarIndex: int}

let make = (): t => {
  tyVarIndex: 0,
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
