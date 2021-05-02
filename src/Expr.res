open Belt

module Const = {
  type t = IntConst(int) | BoolConst(bool) | UnitConst

  let show = c =>
    switch c {
    | IntConst(n) => Int.toString(n)
    | BoolConst(b) => b ? "true" : "false"
    | UnitConst => "()"
    }
}

type rec t =
  | ConstExpr(Const.t)
  | BinOpExpr(t, Token.BinOp.t, t)
  | VarExpr(string)
  | FuncExpr(array<string>, t)
  | LetExpr(string, t)
  | LetInExpr(string, t, t)
  | AppExpr(t, array<t>)
  | BlockExpr(array<t>)
  | IfExpr(t, t, t)

let rec show = expr =>
  switch expr {
  | BinOpExpr(a, op, b) => `(${show(a)} ${Token.BinOp.show(op)} ${show(b)})`
  | ConstExpr(c) => c->Const.show
  | VarExpr(x) => x
  | LetExpr(x, e) => `let ${x} = ${show(e)}`
  | LetInExpr(x, e1, e2) => `let ${x} = ${show(e1)} in ${show(e2)}`
  | FuncExpr(args, body) =>
    switch args {
    | [] => `() -> ${show(body)}`
    | [x] => `${x} -> ${show(body)}`
    | _ => `(${args->Array.joinWith(", ", x => x)}) -> ${show(body)}`
    }
  | IfExpr(cond, thenE, elseE) => `if ${show(cond)} ${show(thenE)} else ${show(elseE)}`
  | AppExpr(f, args) => `(${show(f)})(${args->Array.joinWith(", ", show)})`
  | BlockExpr(exprs) => `{\n${exprs->Array.joinWith("\n", expr => `  ${show(expr)}`)}\n}`
  }
