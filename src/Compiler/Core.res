open Belt
open Types

module CoreExpr = {
  open Expr

  type rec t =
    | CoreConstExpr(monoTy, Const.t)
    | CoreBinOpExpr(monoTy, t, Token.BinOp.t, t)
    | CoreVarExpr(monoTy, string)
    | CoreFuncExpr(monoTy, array<(string, monoTy)>, t)
    | CoreLetInExpr(monoTy, (string, monoTy), t, t)
    | CoreAppExpr(monoTy, t, array<t>)
    | CoreBlockExpr(monoTy, array<t>)
    | CoreIfExpr(monoTy, t, t, t)

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
    | CoreBinOpExpr(tau, a, op, b) =>
      withType(tau, `(${show(a)} ${Token.BinOp.show(op)} ${show(b)})`)
    | CoreVarExpr(tau, x) => withType(tau, x)
    | CoreFuncExpr(tau, args, body) =>
      withType(tau, `(${args->Array.joinWith(", ", ((x, _)) => x)}) -> ${show(body)}`)
    | CoreLetInExpr(tau, (x, _), valExpr, inExpr) =>
      withType(tau, `let ${x} = ${show(valExpr)} in\n${show(inExpr)}`)
    | CoreAppExpr(tau, f, args) =>
      withType(tau, `(${show(f)})(${args->Array.joinWith(", ", show)})`)
    | CoreIfExpr(tau, cond, thenE, elseE) =>
      withType(tau, `if ${show(cond)} ${show(thenE)} else ${show(elseE)}`)
    | CoreBlockExpr(tau, exprs) =>
      withType(tau, "{\n" ++ exprs->Array.joinWith("\n", e => show(e)) ++ "}\n}")
    }
  }

  let tyVarOf = expr => {
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
  }

  let rec subst = (s: Subst.t, expr: t): t => {
    let recCall = subst(s)
    let subst = Subst.substMono(s)
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

  let rec from = (expr: Expr.t): t => {
    let __tau = lazy Context.freshTyVar()
    let tau = _ => Lazy.force(__tau)

    switch expr {
    | ConstExpr(c) => CoreConstExpr(tau(), c)
    | BinOpExpr(a, op, b) => CoreBinOpExpr(tau(), from(a), op, from(b))
    | VarExpr(x) => CoreVarExpr(tau(), x)
    | FuncExpr(args, body) =>
      switch args {
      | [] => CoreFuncExpr(tau(), [("_x", Context.freshTyVar())], from(body))
      | _ => CoreFuncExpr(tau(), args->Array.map(x => (x, Context.freshTyVar())), from(body))
      }
    | LetExpr(x, e) =>
      CoreLetInExpr(tau(), (x, Context.freshTyVar()), from(e), from(ConstExpr(UnitConst)))
    | LetInExpr(x, e1, e2) => CoreLetInExpr(tau(), (x, Context.freshTyVar()), from(e1), from(e2))
    | AppExpr(f, args) => CoreAppExpr(tau(), from(f), args->Array.map(arg => from(arg)))
    | BlockExpr(exprs) =>
      switch exprs {
      // simplify { e1 } into e1
      | [e1] => from(e1)
      | _ => CoreBlockExpr(tau(), exprs->Array.map(from))
      }
    | IfExpr(cond, thenE, elseE) => CoreIfExpr(tau(), from(cond), from(thenE), from(elseE))
    }
  }
}

module CoreDecl = {
  open Decl

  type t =
    | CoreFuncDecl((string, monoTy), array<(string, monoTy)>, CoreExpr.t)
    | CoreLetDecl((string, monoTy), CoreExpr.t)

  let from = decl => {
    switch decl {
    | FuncDecl(f, args, body) => {
        let args = args->Array.map(x => (x, Context.freshTyVar()))
        CoreFuncDecl((f, Context.freshTyVar()), args, CoreExpr.from(body))
      }
    | LetDecl(x, val) => CoreLetDecl((x, Context.freshTyVar()), CoreExpr.from(val))
    }
  }

  let subst = (s: Subst.t, decl: t): t => {
    switch decl {
    | CoreLetDecl((x, xTy), val) =>
      CoreLetDecl((x, Subst.substMono(s, xTy)), CoreExpr.subst(s, val))
    | CoreFuncDecl((f, fTy), args, body) =>
      CoreFuncDecl(
        (f, Subst.substMono(s, fTy)),
        args->Array.map(((x, xTy)) => (x, Subst.substMono(s, xTy))),
        CoreExpr.subst(s, body),
      )
    }
  }

  let show = (~subst=None, decl: t) => {
    let withType = (tau: monoTy, str) => {
      switch subst {
      | Some(s) => `(${str}: ${showMonoTy(Subst.substMono(s, tau))})`
      | None => str
      }
    }

    switch decl {
    | CoreLetDecl((x, xTy), val) => withType(xTy, `let ${x} = ${CoreExpr.show(~subst, val)}`)
    | CoreFuncDecl((f, fTy), args, body) => {
        let args = switch subst {
        | Some(s) =>
          args->Array.joinWith(", ", ((x, xTy)) => `${x}: ${showMonoTy(Subst.substMono(s, xTy))}`)
        | None => args->Array.joinWith(", ", ((x, _)) => x)
        }

        withType(fTy, "fn " ++ f ++ " (" ++ args ++ ") " ++ CoreExpr.show(~subst, body))
      }
    }
  }
}
