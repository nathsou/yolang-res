open Belt

type t = {mod: Wasm.Module.t}

let rec compileExpr = (expr: Core.t): array<Wasm.Inst.t> => {
  open Core
  switch expr {
  | CoreConstExpr(_, c) =>
    switch c {
    | Expr.Const.IntConst(n) => [Wasm.Inst.ConstI32(n)]
    | Expr.Const.BoolConst(b) => [Wasm.Inst.ConstI32(b ? 1 : 0)]
    | Expr.Const.UnitConst => [Wasm.Inst.ConstI32(0)]
    }
  | CoreBinOpExpr(_, lhs, op, rhs) =>
    switch op {
    | Token.BinOp.Plus => Array.concatMany([compileExpr(lhs), compileExpr(rhs), [Wasm.Inst.AddI32]])
    | Token.BinOp.Sub => Array.concatMany([compileExpr(lhs), compileExpr(rhs), [Wasm.Inst.SubI32]])
    | Token.BinOp.Mult => Array.concatMany([compileExpr(lhs), compileExpr(rhs), [Wasm.Inst.MulI32]])
    | Token.BinOp.Div =>
      Array.concatMany([compileExpr(lhs), compileExpr(rhs), [Wasm.Inst.DivI32Unsigned]])
    | Token.BinOp.Eq =>
      switch (lhs, rhs) {
      | (Core.CoreConstExpr(_, Expr.Const.IntConst(0)), _) =>
        Array.concat(compileExpr(rhs), [Wasm.Inst.EqzI32])
      | (_, Core.CoreConstExpr(_, Expr.Const.IntConst(0))) =>
        Array.concat(compileExpr(lhs), [Wasm.Inst.EqzI32])
      | _ => Array.concatMany([compileExpr(lhs), compileExpr(rhs), [Wasm.Inst.EqI32]])
      }
    | Token.BinOp.Neq => Array.concatMany([compileExpr(lhs), compileExpr(rhs), [Wasm.Inst.NeI32]])
    | Token.BinOp.Lss =>
      Array.concatMany([compileExpr(lhs), compileExpr(rhs), [Wasm.Inst.LtI32Unsigned]])
    | Token.BinOp.Leq =>
      Array.concatMany([compileExpr(lhs), compileExpr(rhs), [Wasm.Inst.LeI32Unsigned]])
    | Token.BinOp.Gtr =>
      Array.concatMany([compileExpr(lhs), compileExpr(rhs), [Wasm.Inst.GtI32Unsigned]])
    | Token.BinOp.Geq =>
      Array.concatMany([compileExpr(lhs), compileExpr(rhs), [Wasm.Inst.GeI32Unsigned]])
    | _ => Js.Exn.raiseError(`compileExpr: binop '${Token.BinOp.show(op)}' not handled`)
    }
  | CoreBlockExpr(_, exprs) => Array.concatMany(exprs->Array.map(compileExpr))
  | CoreIfExpr(_, cond, thenE, elseE) =>
    Array.concatMany([
      compileExpr(cond),
      [Wasm.Inst.If(Wasm.BlockReturnType.I32)],
      compileExpr(thenE),
      [Wasm.Inst.Else],
      compileExpr(elseE),
      [Wasm.Inst.End],
    ])
  | _ => Js.Exn.raiseError(`compileExpr: '${Core.show(expr)}' not handled`)
  }
}
