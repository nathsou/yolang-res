open Belt
open Types

module CoreAst = {
  open Ast
  type rec expr =
    | CoreConstExpr(Const.t)
    | CoreBinOpExpr(monoTy, expr, BinOp.t, expr)
    | CoreUnaryOpExpr(monoTy, UnaryOp.t, expr)
    | CoreVarExpr(Context.nameRef)
    | CoreAssignmentExpr(expr, expr)
    | CoreFuncExpr(monoTy, option<Context.nameRef>, array<Context.nameRef>, expr)
    | CoreLetInExpr(monoTy, Context.nameRef, expr, expr)
    | CoreAppExpr(monoTy, expr, array<expr>)
    | CoreBlockExpr(monoTy, array<stmt>, option<expr>, blockSafety)
    | CoreIfExpr(monoTy, expr, expr, expr)
    | CoreWhileExpr(expr, expr)
    | CoreReturnExpr(option<expr>)
    | CoreTypeAssertion(expr, Types.monoTy, Types.monoTy)
    | CoreTupleExpr(array<expr>)
    | CoreStructExpr(string, array<(string, expr)>)
    | CoreAttributeAccessExpr(Types.monoTy, expr, string)
  and stmt = CoreLetStmt(Context.nameRef, bool, expr) | CoreExprStmt(expr)
  and decl =
    | CoreFuncDecl(Context.nameRef, array<(Context.nameRef, bool)>, expr)
    | CoreGlobalDecl(Context.nameRef, bool, expr)
    | CoreStructDecl(string, array<(string, Types.monoTy, bool)>)
    | CoreImplDecl(string, array<(Context.nameRef, array<(Context.nameRef, bool)>, expr)>)

  let rec typeOfExpr = (expr: expr): monoTy => {
    switch expr {
    | CoreConstExpr(c) =>
      switch c {
      | Const.BoolConst(_) => Types.boolTy
      | Const.U32Const(_) => Types.u32Ty
      | Const.UnitConst => Types.unitTy
      }
    | CoreUnaryOpExpr(tau, _, _) => tau
    | CoreBinOpExpr(tau, _, _, _) => tau
    | CoreVarExpr(id) => id.contents.ty
    | CoreAssignmentExpr(_, _) => unitTy
    | CoreFuncExpr(tau, _, _, _) => tau
    | CoreLetInExpr(tau, _, _, _) => tau
    | CoreAppExpr(tau, _, _) => tau
    | CoreBlockExpr(tau, _, _, _) => tau
    | CoreIfExpr(tau, _, _, _) => tau
    | CoreWhileExpr(_, _) => unitTy
    | CoreReturnExpr(expr) => expr->Option.mapWithDefault(unitTy, typeOfExpr)
    | CoreTypeAssertion(_, _, assertedTy) => assertedTy
    | CoreTupleExpr(exprs) => Types.tupleTy(exprs->Array.map(typeOfExpr))
    | CoreStructExpr(name, _) => TyStruct(NamedStruct(name))
    | CoreAttributeAccessExpr(tau, _, _) => tau
    }
  }

  let typeOfStmt = (stmt: stmt): monoTy => {
    switch stmt {
    | CoreExprStmt(expr) => typeOfExpr(expr)
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
    | CoreConstExpr(c) => withType(typeOfExpr(expr), c->Const.show)
    | CoreBinOpExpr(tau, a, op, b) => withType(tau, `(${show(a)} ${BinOp.show(op)} ${show(b)})`)
    | CoreUnaryOpExpr(tau, op, expr) => withType(tau, `(${UnaryOp.show(op)}${show(expr)})`)
    | CoreVarExpr(id) => withType(id.contents.ty, id.contents.name)
    | CoreAssignmentExpr(lhs, rhs) => withType(typeOfExpr(rhs), `${show(lhs)} = ${show(rhs)}`)
    | CoreFuncExpr(tau, _, args, body) =>
      withType(tau, `(${args->Array.joinWith(", ", x => x.contents.name)}) -> ${show(body)}`)
    | CoreLetInExpr(tau, x, valExpr, inExpr) =>
      withType(tau, `let ${x.contents.name} = ${show(valExpr)} in\n${show(inExpr)}`)
    | CoreAppExpr(tau, f, args) =>
      withType(tau, `(${show(f)})(${args->Array.joinWith(", ", show)})`)
    | CoreIfExpr(tau, cond, thenE, elseE) =>
      withType(tau, `if ${show(cond)} ${show(thenE)} else ${show(elseE)}`)
    | CoreBlockExpr(tau, stmts, lastExpr, safety) =>
      withType(
        tau,
        (safety == Unsafe ? "unsafe " : "") ++
        "{\n" ++
        Array.concat(
          stmts->Array.map(showStmt(~subst)),
          lastExpr->Option.mapWithDefault([], e => [showExpr(e)]),
        )->Array.joinWith(";\n", x => "  " ++ x) ++
        (lastExpr->Option.isNone ? "; " : "") ++ "\n}",
      )
    | CoreWhileExpr(cond, body) => withType(unitTy, `while ${showExpr(cond)} ${showExpr(body)}`)
    | CoreReturnExpr(ret) =>
      "return " ++ withType(typeOfExpr(expr), ret->Option.mapWithDefault("", showExpr(~subst)))
    | CoreTypeAssertion(expr, _, assertedTy) =>
      `${withType(typeOfExpr(expr), showExpr(expr))} as ${Types.showMonoTy(assertedTy)}`
    | CoreTupleExpr(exprs) => "(" ++ exprs->Array.joinWith(", ", showExpr(~subst)) ++ ")"
    | CoreStructExpr(name, attrs) =>
      name ++
      " {\n" ++
      attrs->Array.joinWith(",\n", ((attr, val)) => attr ++ ": " ++ showExpr(val)) ++ "\n}"
    | CoreAttributeAccessExpr(tau, lhs, attr) => withType(tau, showExpr(lhs) ++ "." ++ attr)
    }
  }

  and showStmt = (~subst=None, stmt) =>
    switch stmt {
    | CoreLetStmt(x, mut, rhs) =>
      `${mut ? "mut" : "let"} ${x.contents.name} = ${showExpr(rhs, ~subst)}`
    | CoreExprStmt(expr) => showExpr(expr, ~subst)
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
          args->Array.joinWith(", ", ((x, mut)) =>
            (mut ? "mut " : "") ++
            `${x.contents.name}: ${showMonoTy(Subst.substMono(s, x.contents.ty))}`
          )
        | None => args->Array.joinWith(", ", ((x, mut)) => (mut ? "mut " : "") ++ x.contents.name)
        }

        withType(
          f.contents.ty,
          "fn " ++ f.contents.name ++ "(" ++ args ++ ") " ++ showExpr(~subst, body),
        )
      }
    | CoreGlobalDecl(x, mut, init) =>
      withType(
        x.contents.ty,
        `${mut ? "mut" : "let"} ${x.contents.name} = ${showExpr(init, ~subst)}`,
      )
    | CoreStructDecl(name, attrs) => Ast.showDecl(Ast.StructDecl(name, attrs))
    | CoreImplDecl(typeName, funcs) =>
      `impl ${typeName} {\n` ++
      funcs->Array.joinWith("\n\n", ((f, args, body)) =>
        showDecl(CoreFuncDecl(f, args, body))
      ) ++ "\n}"
    }
  }

  exception UnexpectedDeclarationInImplBlock(decl)
  exception UndeclaredIdentifer(string)

  let rec fromExpr = expr => {
    open Context
    let __tau = lazy freshTyVar()
    let tau = _ => Lazy.force(__tau)

    switch expr {
    | Ast.ConstExpr(c) => CoreConstExpr(c)
    | UnaryOpExpr(op, b) => CoreUnaryOpExpr(tau(), op, fromExpr(b))
    | BinOpExpr(a, op, b) => CoreBinOpExpr(tau(), fromExpr(a), op, fromExpr(b))
    | VarExpr(x) =>
      switch getIdentifier(x) {
      | Some(id) => CoreVarExpr(id)
      | None =>
        switch Context.getStruct(x) {
        | Some(_) => CoreVarExpr(Context.freshIdentifier(x))
        | None => raise(UndeclaredIdentifer(x))
        }
      }
    | AssignmentExpr(lhs, rhs) => CoreAssignmentExpr(fromExpr(lhs), fromExpr(rhs))
    | FuncExpr(args, body) =>
      switch args {
      | [] => CoreFuncExpr(tau(), None, [freshIdentifier("__x")], fromExpr(body))
      | _ => CoreFuncExpr(tau(), None, args->Array.map(freshIdentifier(~ty=None)), fromExpr(body))
      }
    | LetInExpr(x, e1, e2) =>
      switch e1 {
      | FuncExpr(args, body) => {
          let f = Context.freshIdentifier(x)
          let func = CoreFuncExpr(
            Context.freshTyVar(),
            Some(f),
            args->Array.map(Context.freshIdentifier(~ty=None)),
            fromExpr(body),
          )

          CoreLetInExpr(tau(), f, func, fromExpr(e2))
        }
      | _ => CoreLetInExpr(tau(), freshIdentifier(x), fromExpr(e1), fromExpr(e2))
      }
    | AppExpr(f, args) => CoreAppExpr(tau(), fromExpr(f), args->Array.map(arg => fromExpr(arg)))
    | BlockExpr(stmts, lastExpr, safety) =>
      CoreBlockExpr(tau(), stmts->Array.map(fromStmt), lastExpr->Option.map(fromExpr), safety)
    | IfExpr(cond, thenE, elseE) =>
      // if the else expression is missing, replace it by unit
      CoreIfExpr(
        tau(),
        fromExpr(cond),
        fromExpr(thenE),
        elseE->Option.mapWithDefault(CoreConstExpr(Const.UnitConst), fromExpr),
      )
    | WhileExpr(cond, body) => CoreWhileExpr(fromExpr(cond), fromExpr(body))
    | ReturnExpr(expr) => CoreReturnExpr(expr->Option.map(fromExpr))
    | TypeAssertion(expr, assertedTy) => CoreTypeAssertion(fromExpr(expr), tau(), assertedTy)
    | TupleExpr(exprs) => CoreTupleExpr(exprs->Array.map(fromExpr))
    | StructExpr(name, attrs) =>
      CoreStructExpr(name, attrs->Array.map(((attr, val)) => (attr, fromExpr(val))))
    | AttributeAccessExpr(lhs, attr) =>
      CoreAttributeAccessExpr(Context.freshTyVar(), fromExpr(lhs), attr)
    }
  }

  and fromStmt = stmt =>
    switch stmt {
    | LetStmt(x, mut, rhs, ty) =>
      switch rhs {
      | FuncExpr(args, body) => {
          let f = Context.freshIdentifier(x, ~ty)
          let func = CoreFuncExpr(
            Context.freshTyVar(),
            Some(f),
            args->Array.map(Context.freshIdentifier(~ty=None)),
            fromExpr(body),
          )

          CoreLetStmt(f, mut, func)
        }
      | _ => CoreLetStmt(Context.freshIdentifier(x, ~ty), mut, fromExpr(rhs))
      }
    | ExprStmt(expr) => CoreExprStmt(fromExpr(expr))
    }

  and fromDecl = decl => {
    switch decl {
    | Ast.FuncDecl(f, args, body) => {
        let args = args->Array.map(((x, mut)) => (Context.freshIdentifier(~ty=None, x), mut))
        CoreFuncDecl(Context.freshIdentifier(f), args, fromExpr(body))
      }
    | Ast.GlobalDecl(x, mut, init) =>
      CoreGlobalDecl(Context.freshIdentifier(x), mut, fromExpr(init))
    | Ast.StructDecl(name, attrs) => {
        // declare this struct
        Context.declareStruct(Context.Struct.make(name, attrs))
        CoreStructDecl(name, attrs)
      }
    | Ast.ImplDecl(typeName, decls) => {
        let funcs =
          decls
          ->Array.map(fromDecl)
          ->Array.map(d =>
            switch d {
            | CoreFuncDecl(f, args, body) => (f, args, body)
            | _ => raise(UnexpectedDeclarationInImplBlock(d))
            }
          )

        CoreImplDecl(typeName, funcs)
      }
    }
  }

  let rec substExpr = (s: Subst.t, expr: expr): expr => {
    let go = substExpr(s)
    let subst = Subst.substMono(s)
    switch expr {
    | CoreConstExpr(c) => CoreConstExpr(c)
    | CoreUnaryOpExpr(tau, op, expr) => CoreUnaryOpExpr(subst(tau), op, go(expr))
    | CoreBinOpExpr(tau, a, op, b) => CoreBinOpExpr(subst(tau), go(a), op, go(b))
    | CoreVarExpr(x) => CoreVarExpr(x)
    | CoreAssignmentExpr(lhs, rhs) => CoreAssignmentExpr(go(lhs), go(rhs))
    | CoreFuncExpr(tau, name, args, e) => CoreFuncExpr(subst(tau), name, args, go(e))
    | CoreLetInExpr(tau, x, e1, e2) => CoreLetInExpr(subst(tau), x, go(e1), go(e2))
    | CoreAppExpr(tau, lhs, args) => CoreAppExpr(subst(tau), go(lhs), args->Array.map(go))
    | CoreBlockExpr(tau, exprs, lastExpr, safety) =>
      CoreBlockExpr(subst(tau), exprs->Array.map(substStmt(s)), lastExpr->Option.map(go), safety)
    | CoreIfExpr(tau, cond, thenE, elseE) => CoreIfExpr(subst(tau), go(cond), go(thenE), go(elseE))
    | CoreWhileExpr(cond, body) => CoreWhileExpr(go(cond), go(body))
    | CoreReturnExpr(expr) => CoreReturnExpr(expr->Option.map(go))
    | CoreTypeAssertion(expr, originalTy, assertedTy) =>
      CoreTypeAssertion(go(expr), subst(originalTy), subst(assertedTy))
    | CoreTupleExpr(exprs) => CoreTupleExpr(exprs->Array.map(go))
    | CoreStructExpr(name, attrs) =>
      // reorder attributes to match the struct declaration
      switch Context.getStruct(name) {
      | Some({attributes}) => {
          // O(len(attributes)^2) but who cares in this case?
          let res =
            attributes
            ->Array.keep(({impl}) => impl->Option.isNone)
            ->Array.map(({name: attrName}) => attrs->Array.getBy(((name, _)) => name == attrName))
            ->ArrayUtils.mapOption(((attrName, val)) => (attrName, go(val)))

          switch res {
          | Some(attrs) => CoreStructExpr(name, attrs)
          | None => Js.Exn.raiseError(`unreachable: missing attribute for struct "${name}"`)
          }
        }
      | None => Js.Exn.raiseError(`unreachable: undeclared struct "${name}'`)
      }
    | CoreAttributeAccessExpr(tau, lhs, attr) => CoreAttributeAccessExpr(subst(tau), go(lhs), attr)
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
    | CoreGlobalDecl(x, mut, init) => CoreGlobalDecl(x, mut, substExpr(s, init))
    | CoreStructDecl(_) => decl
    | CoreImplDecl(typeName, funcs) =>
      CoreImplDecl(typeName, funcs->Array.map(((f, args, body)) => (f, args, substExpr(s, body))))
    }
  }
}

module CoreExpr = {
  type rec t = CoreAst.expr

  let show = CoreAst.showExpr
  let from = CoreAst.fromExpr
  let subst = CoreAst.substExpr
  let typeOf = CoreAst.typeOfExpr
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
  let typeOf = CoreAst.typeOfStmt
}
