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
      Array.concatMany([compileExpr(lhs), compileExpr(rhs), [Wasm.Inst.DivI32Signed]])
    | _ => Js.Exn.raiseError(`compileExpr: binop '${Token.BinOp.show(op)}' not handled`)
    }
  | _ => Js.Exn.raiseError(`compileExpr: '${Core.show(expr)}' not handled`)
  }
}
