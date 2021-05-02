open Belt
open Types

module CoreAst = {
  open Ast
  type rec expr =
    | CoreConstExpr(monoTy, Ast.Const.t)
    | CoreBinOpExpr(monoTy, expr, Token.BinOp.t, expr)
    | CoreVarExpr(monoTy, string)
    | CoreAssignmentExpr(string, expr)
    | CoreFuncExpr(monoTy, array<(string, monoTy)>, expr)
    | CoreLetInExpr(monoTy, (string, monoTy), expr, expr)
    | CoreAppExpr(monoTy, expr, array<expr>)
    | CoreBlockExpr(monoTy, array<stmt>, option<expr>)
    | CoreIfExpr(monoTy, expr, expr, expr)
  and decl = CoreFuncDecl((string, monoTy), array<(string, monoTy)>, expr)
  and stmt = CoreLetStmt((string, monoTy), bool, expr) | CoreExprStmt(expr)

  let tyVarOfExpr = (expr: expr): monoTy => {
    switch expr {
    | CoreConstExpr(tau, _) => tau
    | CoreBinOpExpr(tau, _, _, _) => tau
    | CoreVarExpr(tau, _) => tau
    | CoreAssignmentExpr(_, _) => unitTy
    | CoreFuncExpr(tau, _, _) => tau
    | CoreLetInExpr(tau, _, _, _) => tau
    | CoreAppExpr(tau, _, _) => tau
    | CoreBlockExpr(tau, _, _) => tau
    | CoreIfExpr(tau, _, _, _) => tau
    }
  }

  let tyVarOfStmt = (stmt: stmt): monoTy => {
    switch stmt {
    | CoreExprStmt(expr) => tyVarOfExpr(expr)
    | CoreLetStmt(_, _, _) => unitTy
    }
  }

  let rec showExpr = (~subst=None, expr): string => {
    let withType = (tau: monoTy, str) => {
      switch subst {
      | Some(s) => `(${str}: ${showMonoTy(Subst.substMono(s, tau))})`
      | None => str
      }
    }

    let show = arg => showExpr(arg, ~subst)

    switch expr {
    | CoreConstExpr(tau, c) => withType(tau, c->Ast.Const.show)
    | CoreBinOpExpr(tau, a, op, b) =>
      withType(tau, `(${show(a)} ${Token.BinOp.show(op)} ${show(b)})`)
    | CoreVarExpr(tau, x) => withType(tau, x)
    | CoreAssignmentExpr(x, val) => withType(tyVarOfExpr(val), `${x} = ${show(val)}`)
    | CoreFuncExpr(tau, args, body) =>
      withType(tau, `(${args->Array.joinWith(", ", ((x, _)) => x)}) -> ${show(body)}`)
    | CoreLetInExpr(tau, (x, _), valExpr, inExpr) =>
      withType(tau, `let ${x} = ${show(valExpr)} in\n${show(inExpr)}`)
    | CoreAppExpr(tau, f, args) =>
      withType(tau, `(${show(f)})(${args->Array.joinWith(", ", show)})`)
    | CoreIfExpr(tau, cond, thenE, elseE) =>
      withType(tau, `if ${show(cond)} ${show(thenE)} else ${show(elseE)}`)
    | CoreBlockExpr(tau, stmts, lastExpr) =>
      withType(
        tau,
        "{\n" ++
        Array.concat(
          stmts->Array.map(showStmt),
          lastExpr->Option.mapWithDefault([], e => [showExpr(e)]),
        )->Array.joinWith(";\n", x => "  " ++ x) ++
        (lastExpr->Option.isNone ? "; " : "") ++ "\n}",
      )
    }
  }

  and showStmt = stmt =>
    switch stmt {
    | CoreLetStmt((x, _), mut, rhs) =>
      if mut {
        `let mut ${x} = ${showExpr(rhs)}`
      } else {
        `let ${x} = ${showExpr(rhs)}`
      }
    | CoreExprStmt(expr) => showExpr(expr)
    }

  and showDecl = (~subst=None, decl: decl): string => {
    let withType = (tau: monoTy, str) => {
      switch subst {
      | Some(s) => `(${str}: ${showMonoTy(Subst.substMono(s, tau))})`
      | None => str
      }
    }

    switch decl {
    | CoreFuncDecl((f, fTy), args, body) => {
        let args = switch subst {
        | Some(s) =>
          args->Array.joinWith(", ", ((x, xTy)) => `${x}: ${showMonoTy(Subst.substMono(s, xTy))}`)
        | None => args->Array.joinWith(", ", ((x, _)) => x)
        }

        withType(fTy, "fn " ++ f ++ "(" ++ args ++ ") " ++ showExpr(~subst, body))
      }
    }
  }

  let rec fromExpr = expr => {
    let __tau = lazy Context.freshTyVar()
    let tau = _ => Lazy.force(__tau)

    switch expr {
    | Ast.ConstExpr(c) => CoreConstExpr(tau(), c)
    | BinOpExpr(a, op, b) => CoreBinOpExpr(tau(), fromExpr(a), op, fromExpr(b))
    | VarExpr(x) => CoreVarExpr(tau(), x)
    | AssignmentExpr(x, val) => CoreAssignmentExpr(x, fromExpr(val))
    | FuncExpr(args, body) =>
      switch args {
      | [] => CoreFuncExpr(tau(), [("_x", Context.freshTyVar())], fromExpr(body))
      | _ => CoreFuncExpr(tau(), args->Array.map(x => (x, Context.freshTyVar())), fromExpr(body))
      }
    | LetInExpr(x, e1, e2) =>
      CoreLetInExpr(tau(), (x, Context.freshTyVar()), fromExpr(e1), fromExpr(e2))
    | AppExpr(f, args) => CoreAppExpr(tau(), fromExpr(f), args->Array.map(arg => fromExpr(arg)))
    | BlockExpr(stmts, lastExpr) =>
      switch (stmts, lastExpr) {
      // simplify { e1 } into e1
      // | ([], Some(e1)) => fromExpr(e1)
      | _ => CoreBlockExpr(tau(), stmts->Array.map(fromStmt), lastExpr->Option.map(fromExpr))
      }
    | IfExpr(cond, thenE, elseE) =>
      CoreIfExpr(tau(), fromExpr(cond), fromExpr(thenE), fromExpr(elseE))
    }
  }

  and fromStmt = stmt =>
    switch stmt {
    | LetStmt(x, mut, rhs) => CoreLetStmt((x, Context.freshTyVar()), mut, fromExpr(rhs))
    | ExprStmt(expr) => CoreExprStmt(fromExpr(expr))
    }

  and fromDecl = decl => {
    switch decl {
    | Ast.FuncDecl(f, args, body) => {
        let args = args->Array.map(x => (x, Context.freshTyVar()))
        CoreFuncDecl((f, Context.freshTyVar()), args, fromExpr(body))
      }
    }
  }

  let rec substExpr = (s: Subst.t, expr: expr): expr => {
    let recCall = substExpr(s)
    let subst = Subst.substMono(s)
    switch expr {
    | CoreConstExpr(tau, c) => CoreConstExpr(subst(tau), c)
    | CoreBinOpExpr(tau, a, op, b) => CoreBinOpExpr(subst(tau), recCall(a), op, recCall(b))
    | CoreVarExpr(tau, x) => CoreVarExpr(subst(tau), x)
    | CoreAssignmentExpr(x, val) => CoreAssignmentExpr(x, recCall(val))
    | CoreFuncExpr(tau, args, e) =>
      CoreFuncExpr(subst(tau), args->Array.map(((x, xTy)) => (x, subst(xTy))), recCall(e))
    | CoreLetInExpr(tau, (x, xTy), e1, e2) =>
      CoreLetInExpr(subst(tau), (x, subst(xTy)), recCall(e1), recCall(e2))
    | CoreAppExpr(tau, lhs, args) => CoreAppExpr(subst(tau), recCall(lhs), args->Array.map(recCall))
    | CoreBlockExpr(tau, exprs, lastExpr) =>
      CoreBlockExpr(subst(tau), exprs->Array.map(substStmt(s)), lastExpr->Option.map(recCall))
    | CoreIfExpr(tau, cond, thenE, elseE) =>
      CoreIfExpr(subst(tau), recCall(cond), recCall(thenE), recCall(elseE))
    }
  }

  and substStmt = (s: Subst.t, stmt: stmt): stmt => {
    switch stmt {
    | CoreExprStmt(expr) => CoreExprStmt(substExpr(s, expr))
    | CoreLetStmt((x, xTy), mut, rhs) =>
      CoreLetStmt((x, Subst.substMono(s, xTy)), mut, substExpr(s, rhs))
    }
  }

  and substDecl = (s: Subst.t, decl: decl): decl => {
    switch decl {
    | CoreFuncDecl((f, fTy), args, body) =>
      CoreFuncDecl(
        (f, Subst.substMono(s, fTy)),
        args->Array.map(((x, xTy)) => (x, Subst.substMono(s, xTy))),
        substExpr(s, body),
      )
    }
  }
}

module CoreExpr = {
  type rec t = CoreAst.expr

  let show = CoreAst.showExpr
  let from = CoreAst.fromExpr
  let subst = CoreAst.substExpr
  let tyVarOf = CoreAst.tyVarOfExpr
}

module CoreDecl = {
  type t = CoreAst.decl

  let show = CoreAst.showDecl
  let from = CoreAst.fromDecl
  let subst = CoreAst.substDecl
}

module CoreStmt = {
  type t = CoreAst.stmt

  let show = CoreAst.showStmt
  let from = CoreAst.fromStmt
  let subst = CoreAst.substStmt
  let tyVarOf = CoreAst.tyVarOfStmt
}
