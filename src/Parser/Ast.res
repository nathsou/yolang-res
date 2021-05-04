open Belt

module Ast = {
  module Const = {
    type t = U32Const(int) | BoolConst(bool) | UnitConst

    let show = c =>
      switch c {
      | U32Const(n) => Int.toString(n)
      | BoolConst(b) => b ? "true" : "false"
      | UnitConst => "()"
      }
  }

  type rec expr =
    | ConstExpr(Const.t)
    | BinOpExpr(expr, Token.BinOp.t, expr)
    | VarExpr(string)
    | AssignmentExpr(string, expr)
    | FuncExpr(array<string>, expr)
    | LetInExpr(string, expr, expr)
    | AppExpr(expr, array<expr>)
    | BlockExpr(array<stmt>, option<expr>)
    | IfExpr(expr, expr, option<expr>)
    | WhileExpr(expr, expr)
    | ReturnExpr(expr)
  and stmt = LetStmt(string, bool, expr) | ExprStmt(expr)
  and decl = FuncDecl(string, array<string>, expr)

  let rec showExpr = expr =>
    switch expr {
    | BinOpExpr(a, op, b) => `(${showExpr(a)} ${Token.BinOp.show(op)} ${showExpr(b)})`
    | ConstExpr(c) => c->Const.show
    | VarExpr(x) => x
    | AssignmentExpr(x, val) => `${x} = ${showExpr(val)}`
    | LetInExpr(x, e1, e2) => `let ${x} = ${showExpr(e1)} in ${showExpr(e2)}`
    | FuncExpr(args, body) =>
      switch args {
      | [] => `() -> ${showExpr(body)}`
      | [x] => `${x} -> ${showExpr(body)}`
      | _ => `(${args->Array.joinWith(", ", x => x)}) -> ${showExpr(body)}`
      }
    | IfExpr(cond, thenExpr, elseExpr) => {
        let head = `if ${showExpr(cond)} ${showExpr(thenExpr)}`
        switch elseExpr {
        | Some(elseExpr) => `${head} else ${showExpr(elseExpr)}`
        | None => head
        }
      }
    | AppExpr(f, args) => `(${showExpr(f)})(${args->Array.joinWith(", ", showExpr)})`
    | BlockExpr(stmts, lastExpr) =>
      "{\n" ++
      Array.concat(
        stmts->Array.map(showStmt),
        lastExpr->Option.mapWithDefault([], e => [showExpr(e)]),
      )->Array.joinWith(";\n", str => `  ${str}`) ++ "}\n}"
    | WhileExpr(cond, body) => `while ${showExpr(cond)} ${showExpr(body)}`
    | ReturnExpr(ret) => `return ${showExpr(ret)}`
    }

  and showDecl = decl =>
    switch decl {
    | FuncDecl(f, args, body) => `fn ${f}(${args->Array.joinWith(", ", x => x)}) ${showExpr(body)}`
    }
  and showStmt = stmt =>
    switch stmt {
    | LetStmt(x, mut, rhs) =>
      if mut {
        `let mut ${x} = ${showExpr(rhs)}`
      } else {
        `let ${x} = ${showExpr(rhs)}`
      }
    | ExprStmt(expr) => showExpr(expr) ++ ";"
    }
}

module Expr = {
  type t = Ast.expr
  let show = Ast.showExpr
  module Const = Ast.Const
}

module Decl = {
  type t = Ast.decl
  let show = Ast.showDecl
}

module Stmt = {
  type t = Ast.stmt
  let show = Ast.showStmt
}
