open Belt
open Expr
open Types

type rec t =
  | CoreConstExpr(monoTy, Const.t)
  | CoreBinOpExpr(monoTy, t, Token.BinOp.t, t)
  | CoreVarExpr(monoTy, string)
  | CoreFuncExpr(monoTy, array<(string, monoTy)>, t)
  | CoreLetInExpr(monoTy, (string, monoTy), t, t)
  | CoreAppExpr(monoTy, t, array<t>)
  | CoreBlockExpr(monoTy, array<t>)
  | CoreIfExpr(monoTy, t, t, t)

let rec fromExpr = expr => {
  // some conversions generate their own fresh type variables
  let __tau = lazy Context.freshTyVar()
  let tau = _ => Lazy.force(__tau)

  switch expr {
  | ConstExpr(c) => CoreConstExpr(tau(), c)
  | BinOpExpr(a, op, b) => CoreBinOpExpr(tau(), fromExpr(a), op, fromExpr(b))
  | VarExpr(x) => CoreVarExpr(tau(), x)
  | FuncExpr(args, body) =>
    switch args {
    | [] => CoreFuncExpr(tau(), [("_x", Context.freshTyVar())], fromExpr(body))
    | _ => CoreFuncExpr(tau(), args->Array.map(x => (x, Context.freshTyVar())), fromExpr(body))
    }
  | LetExpr(x, e) =>
    CoreLetInExpr(tau(), (x, Context.freshTyVar()), fromExpr(e), fromExpr(ConstExpr(UnitConst)))
  | LetInExpr(x, e1, e2) =>
    CoreLetInExpr(tau(), (x, Context.freshTyVar()), fromExpr(e1), fromExpr(e2))
  | AppExpr(f, args) => CoreAppExpr(tau(), fromExpr(f), args->Array.map(arg => fromExpr(arg)))
  | BlockExpr(exprs) =>
    switch exprs {
    // simplify { e1 } into e1
    | list{e1} => fromExpr(e1)
    | _ => {
        let rec rewriteLets = (exprs: list<Expr.t>, acc: list<t>) =>
          switch exprs {
          | list{LetExpr(x, e), ...tl} => {
              let tau2 = Context.freshTyVar()
              rewriteLets(
                list{},
                list{
                  CoreLetInExpr(
                    tau2,
                    (x, Context.freshTyVar()),
                    fromExpr(e),
                    fromExpr(BlockExpr(tl)),
                  ),
                  ...acc,
                },
              )
            }
          | list{expr, ...tl} => rewriteLets(tl, list{fromExpr(expr), ...acc})
          | list{} => acc
          }

        CoreBlockExpr(tau(), rewriteLets(exprs, list{})->List.toArray->Array.reverse)
      }
    }
  | IfExpr(cond, thenE, elseE) =>
    CoreIfExpr(tau(), fromExpr(cond), fromExpr(thenE), fromExpr(elseE))
  }
}

let tyVarOf = expr =>
  switch expr {
  | CoreConstExpr(tau, _) => tau
  | CoreBinOpExpr(tau, _, _, _) => tau
  | CoreVarExpr(tau, _) => tau
  | CoreFuncExpr(tau, _, _) => tau
  | CoreLetInExpr(tau, _, _, _) => tau
  | CoreAppExpr(tau, _, _) => tau
  | CoreBlockExpr(tau, _) => tau
  | CoreIfExpr(tau, _, _, _) => tau
  }

let rec substt = (subst, expr) => {
  let recCall = substt(subst)
  let subst = Subst.substMono(subst)
  switch expr {
  | CoreConstExpr(tau, c) => CoreConstExpr(subst(tau), c)
  | CoreBinOpExpr(tau, a, op, b) => CoreBinOpExpr(subst(tau), recCall(a), op, recCall(b))
  | CoreVarExpr(tau, x) => CoreVarExpr(subst(tau), x)
  | CoreFuncExpr(tau, args, e) =>
    CoreFuncExpr(subst(tau), args->Array.map(((x, xTy)) => (x, subst(xTy))), recCall(e))
  | CoreLetInExpr(tau, (x, xTy), e1, e2) =>
    CoreLetInExpr(subst(tau), (x, subst(xTy)), recCall(e1), recCall(e2))
  | CoreAppExpr(tau, lhs, args) => CoreAppExpr(subst(tau), recCall(lhs), args->Array.map(recCall))
  | CoreBlockExpr(tau, exprs) => CoreBlockExpr(subst(tau), exprs->Array.map(recCall))
  | CoreIfExpr(tau, cond, thenE, elseE) =>
    CoreIfExpr(subst(tau), recCall(cond), recCall(thenE), recCall(elseE))
  }
}

let rec show = (~subst=None, expr) => {
  let withType = (tau: monoTy, str) => {
    switch subst {
    | Some(s) => `(${str}: ${showMonoTy(Subst.substMono(s, tau))})`
    | None => str
    }
  }

  let show = arg => show(arg, ~subst)

  switch expr {
  | CoreConstExpr(tau, c) => withType(tau, c->Const.show)
  | CoreBinOpExpr(tau, a, op, b) => withType(tau, `(${show(a)} ${Token.BinOp.show(op)} ${show(b)})`)
  | CoreVarExpr(tau, x) => withType(tau, x)
  | CoreFuncExpr(tau, args, body) =>
    withType(tau, `(${args->Array.joinWith(", ", ((x, _)) => x)}) -> ${show(body)}`)
  | CoreLetInExpr(tau, (x, _), valExpr, inExpr) =>
    withType(tau, `let ${x} = ${show(valExpr)} in\n${show(inExpr)}`)
  | CoreAppExpr(tau, f, args) => withType(tau, `(${show(f)})(${args->Array.joinWith(", ", show)})`)
  | CoreIfExpr(tau, cond, thenE, elseE) =>
    withType(tau, `if ${show(cond)} ${show(thenE)} else ${show(elseE)}`)
  | CoreBlockExpr(tau, exprs) =>
    withType(tau, `{\n${exprs->Array.joinWith("\n", e => `  ${show(e)}`)}\n}`)
  }
}
