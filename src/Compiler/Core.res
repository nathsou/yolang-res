open Belt
open Types

module CoreAst = {
  open Ast
  type rec expr =
    | CoreConstExpr(monoTy, Ast.Const.t)
    | CoreBinOpExpr(monoTy, expr, Token.BinOp.t, expr)
    | CoreVarExpr(Context.nameRef)
    | CoreAssignmentExpr(Context.nameRef, expr)
    | CoreFuncExpr(monoTy, option<Context.nameRef>, array<Context.nameRef>, expr)
    | CoreLetInExpr(monoTy, Context.nameRef, expr, expr)
    | CoreAppExpr(monoTy, expr, array<expr>)
    | CoreBlockExpr(monoTy, array<stmt>, option<expr>)
    | CoreIfExpr(monoTy, expr, expr, expr)
    | CoreWhileExpr(expr, expr)
    | CoreReturnExpr(expr)
  and decl = CoreFuncDecl(Context.nameRef, array<Context.nameRef>, expr)
  and stmt = CoreLetStmt(Context.nameRef, bool, expr) | CoreExprStmt(expr)

  let rec tyVarOfExpr = (expr: expr): monoTy => {
    switch expr {
    | CoreConstExpr(tau, _) => tau
    | CoreBinOpExpr(tau, _, _, _) => tau
    | CoreVarExpr(id) => id.contents.ty
    | CoreAssignmentExpr(_, _) => unitTy
    | CoreFuncExpr(tau, _, _, _) => tau
    | CoreLetInExpr(tau, _, _, _) => tau
    | CoreAppExpr(tau, _, _) => tau
    | CoreBlockExpr(tau, _, _) => tau
    | CoreIfExpr(tau, _, _, _) => tau
    | CoreWhileExpr(_, _) => unitTy
    | CoreReturnExpr(expr) => tyVarOfExpr(expr)
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
    | CoreVarExpr(id) => withType(id.contents.ty, id.contents.name)
    | CoreAssignmentExpr(x, val) => withType(tyVarOfExpr(val), `${x.contents.name} = ${show(val)}`)
    | CoreFuncExpr(tau, _, args, body) =>
      withType(tau, `(${args->Array.joinWith(", ", x => x.contents.name)}) -> ${show(body)}`)
    | CoreLetInExpr(tau, x, valExpr, inExpr) =>
      withType(tau, `let ${x.contents.name} = ${show(valExpr)} in\n${show(inExpr)}`)
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
    | CoreWhileExpr(cond, body) => withType(unitTy, `while ${showExpr(cond)} ${showExpr(body)}`)
    | CoreReturnExpr(expr) => `return ${showExpr(expr)}`
    }
  }

  and showStmt = stmt =>
    switch stmt {
    | CoreLetStmt(x, mut, rhs) =>
      if mut {
        `let mut ${x.contents.name} = ${showExpr(rhs)}`
      } else {
        `let ${x.contents.name} = ${showExpr(rhs)}`
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
    | CoreFuncDecl(f, args, body) => {
        let args = switch subst {
        | Some(s) =>
          args->Array.joinWith(", ", x =>
            `${x.contents.name}: ${showMonoTy(Subst.substMono(s, x.contents.ty))}`
          )
        | None => args->Array.joinWith(", ", x => x.contents.name)
        }

        withType(
          f.contents.ty,
          "fn " ++ f.contents.name ++ "(" ++ args ++ ") " ++ showExpr(~subst, body),
        )
      }
    }
  }

  let rec fromExpr = expr => {
    open Context
    let __tau = lazy freshTyVar()
    let tau = _ => Lazy.force(__tau)

    switch expr {
    | Ast.ConstExpr(c) => CoreConstExpr(tau(), c)
    | BinOpExpr(a, op, b) => CoreBinOpExpr(tau(), fromExpr(a), op, fromExpr(b))
    | VarExpr(x) => CoreVarExpr(getIdentifier(x))
    | AssignmentExpr(x, val) => CoreAssignmentExpr(getIdentifier(x), fromExpr(val))
    | FuncExpr(args, body) =>
            switch args {
      | [] => CoreFuncExpr(tau(), None, [freshIdentifier("__x")], fromExpr(body))
      | _ => CoreFuncExpr(tau(), None, args->Array.map(freshIdentifier), fromExpr(body))
      }
    | LetInExpr(x, e1, e2) => switch e1 {
      | FuncExpr(args, body) => {
        let f = Context.freshIdentifier(x)
        let func = CoreFuncExpr(
          Context.freshTyVar(),
          Some(f),
          args->Array.map(Context.freshIdentifier),
          fromExpr(body),
        )

        CoreLetInExpr(tau(), f, func, fromExpr(e2))
      }
      | _ => CoreLetInExpr(tau(), freshIdentifier(x), fromExpr(e1), fromExpr(e2))
    }
    | AppExpr(f, args) => CoreAppExpr(tau(), fromExpr(f), args->Array.map(arg => fromExpr(arg)))
    | BlockExpr(stmts, lastExpr) =>
      switch (stmts, lastExpr) {
      // simplify { e1 } into e1
      | ([], Some(e1)) => fromExpr(e1)
      | _ => CoreBlockExpr(tau(), stmts->Array.map(fromStmt), lastExpr->Option.map(fromExpr))
      }
    | IfExpr(cond, thenE, elseE) =>
      // if the else expression is missing, replace it by unit
      CoreIfExpr(
        tau(),
        fromExpr(cond),
        fromExpr(thenE),
        elseE->Option.mapWithDefault(CoreConstExpr(unitTy, Ast.Const.UnitConst), fromExpr),
      )
    | WhileExpr(cond, body) => CoreWhileExpr(fromExpr(cond), fromExpr(body))
    | ReturnExpr(expr) => CoreReturnExpr(fromExpr(expr))
    }
  }

  and fromStmt = stmt =>
    switch stmt {
    | LetStmt(x, mut, rhs) =>
      switch rhs {
      | FuncExpr(args, body) => {
          let f = Context.freshIdentifier(x)
          let func = CoreFuncExpr(
            Context.freshTyVar(),
            Some(f),
            args->Array.map(Context.freshIdentifier),
            fromExpr(body),
          )

          CoreLetStmt(f, mut, func)
        }
      | _ => CoreLetStmt(Context.freshIdentifier(x), mut, fromExpr(rhs))
      }
    | ExprStmt(expr) => CoreExprStmt(fromExpr(expr))
    }

  and fromDecl = decl => {
    switch decl {
    | Ast.FuncDecl(f, args, body) => {
        let args = args->Array.map(Context.freshIdentifier)
        CoreFuncDecl(Context.freshIdentifier(f), args, fromExpr(body))
      }
    }
  }

  let rec substExpr = (s: Subst.t, expr: expr): expr => {
    let recCall = substExpr(s)
    let subst = Subst.substMono(s)
    switch expr {
    | CoreConstExpr(tau, c) => CoreConstExpr(subst(tau), c)
    | CoreBinOpExpr(tau, a, op, b) => CoreBinOpExpr(subst(tau), recCall(a), op, recCall(b))
    | CoreVarExpr(x) => CoreVarExpr(x)
    | CoreAssignmentExpr(x, val) => CoreAssignmentExpr(x, recCall(val))
    | CoreFuncExpr(tau, name, args, e) => CoreFuncExpr(subst(tau), name, args, recCall(e))
    | CoreLetInExpr(tau, x, e1, e2) => CoreLetInExpr(subst(tau), x, recCall(e1), recCall(e2))
    | CoreAppExpr(tau, lhs, args) => CoreAppExpr(subst(tau), recCall(lhs), args->Array.map(recCall))
    | CoreBlockExpr(tau, exprs, lastExpr) =>
      CoreBlockExpr(subst(tau), exprs->Array.map(substStmt(s)), lastExpr->Option.map(recCall))
    | CoreIfExpr(tau, cond, thenE, elseE) =>
      CoreIfExpr(subst(tau), recCall(cond), recCall(thenE), recCall(elseE))
    | CoreWhileExpr(cond, body) => CoreWhileExpr(recCall(cond), recCall(body))
    | CoreReturnExpr(expr) => CoreReturnExpr(recCall(expr))
    }
  }

  and substStmt = (s: Subst.t, stmt: stmt): stmt => {
    switch stmt {
    | CoreExprStmt(expr) => CoreExprStmt(substExpr(s, expr))
    | CoreLetStmt(x, mut, rhs) => CoreLetStmt(x, mut, substExpr(s, rhs))
    }
  }

  and substDecl = (s: Subst.t, decl: decl): decl => {
    switch decl {
    | CoreFuncDecl(f, args, body) => CoreFuncDecl(f, args, substExpr(s, body))
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
