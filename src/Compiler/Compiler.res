open Belt

module Local = {
  type t = {
    name: string,
    depth: int,
    ty: Types.monoTy,
  }

  let make = (name, depth, ty): t => {name: name, depth: depth, ty: ty}
}

let wasmValueTyOf = (tau: Types.monoTy): Wasm.ValueType.t => {
  open Types
  open Wasm.ValueType
  switch tau {
  | TyConst("u32", []) => I32
  | TyConst("u64", []) => I64
  | TyConst("bool", []) => I32
  | TyConst("()", []) => I32
  | _ => Js.Exn.raiseError(`wasmValueTyOf: cannot convert ${showMonoTy(tau)}`)
  }
}

module Func = {
  type t = {
    name: string,
    locals: array<Local.t>,
    params: array<Types.monoTy>,
    ret: Types.monoTy,
    instructions: array<Wasm.Inst.t>,
  }

  let emit = (self: t, inst: Wasm.Inst.t): unit => {
    let _ = self.instructions->Js.Array2.push(inst)
  }

  let toWasmFunc = (self: t): Wasm.Func.t => {
    let sig = Wasm.Func.Signature.make(
      self.params->Array.map(wasmValueTyOf),
      Some(self.ret->wasmValueTyOf),
    )

    let locals = Wasm.Func.Locals.fromTypes(self.locals->Array.map(l => wasmValueTyOf(l.ty)))

    let body = Wasm.Func.Body.make(locals, self.instructions)

    Wasm.Func.make(sig, body)
  }
}

type t = {
  mod: Wasm.Module.t,
  funcs: MutableStack.t<Func.t>,
  mutable localCount: int,
  mutable scopeDepth: int,
}

let emit = (self: t, inst: Wasm.Inst.t): unit => {
  switch self.funcs->MutableStack.top {
  | Some(f) => f->Func.emit(inst)
  | None => Js.Exn.raiseError(`tried to emit an instruction on an empty function stack`)
  }
}

let rec compileExpr = (self: t, expr: Core.t): unit => {
  open Core
  switch expr {
  | CoreConstExpr(_, c) => {
      let inst = switch c {
      | Expr.Const.IntConst(n) => Wasm.Inst.ConstI32(n)
      | Expr.Const.BoolConst(b) => Wasm.Inst.ConstI32(b ? 1 : 0)
      | Expr.Const.UnitConst => Wasm.Inst.ConstI32(0)
      }

      self->emit(inst)
    }
  | CoreBinOpExpr(_, lhs, op, rhs) =>
    switch op {
    | Token.BinOp.Plus => {
        self->compileExpr(lhs)
        self->compileExpr(rhs)
        self->emit(Wasm.Inst.AddI32)
      }
    | Token.BinOp.Sub => {
        self->compileExpr(lhs)
        self->compileExpr(rhs)
        self->emit(Wasm.Inst.SubI32)
      }
    | Token.BinOp.Mult => {
        self->compileExpr(lhs)
        self->compileExpr(rhs)
        self->emit(Wasm.Inst.MulI32)
      }
    | Token.BinOp.Div => {
        self->compileExpr(lhs)
        self->compileExpr(rhs)
        self->emit(Wasm.Inst.DivI32Unsigned)
      }
    | Token.BinOp.Eq =>
      switch (lhs, rhs) {
      | (Core.CoreConstExpr(_, Expr.Const.IntConst(0)), _) => {
          self->compileExpr(rhs)
          self->emit(Wasm.Inst.EqzI32)
        }
      | (_, Core.CoreConstExpr(_, Expr.Const.IntConst(0))) => {
          self->compileExpr(lhs)
          self->emit(Wasm.Inst.EqzI32)
        }
      | _ => {
          self->compileExpr(lhs)
          self->compileExpr(rhs)
          self->emit(Wasm.Inst.EqI32)
        }
      }
    | Token.BinOp.Neq => {
        self->compileExpr(lhs)
        self->compileExpr(rhs)
        self->emit(Wasm.Inst.NeI32)
      }
    | Token.BinOp.Lss => {
        self->compileExpr(lhs)
        self->compileExpr(rhs)
        self->emit(Wasm.Inst.LtI32Unsigned)
      }
    | Token.BinOp.Leq => {
        self->compileExpr(lhs)
        self->compileExpr(rhs)
        self->emit(Wasm.Inst.LeI32Unsigned)
      }
    | Token.BinOp.Gtr => {
        self->compileExpr(lhs)
        self->compileExpr(rhs)
        self->emit(Wasm.Inst.GtI32Unsigned)
      }
    | Token.BinOp.Geq => {
        self->compileExpr(lhs)
        self->compileExpr(rhs)
        self->emit(Wasm.Inst.GeI32Unsigned)
      }
    | _ => Js.Exn.raiseError(`compileExpr: binop '${Token.BinOp.show(op)}' not handled`)
    }
  | CoreBlockExpr(_, exprs) => exprs->Js.Array2.forEach(self->compileExpr)
  | CoreIfExpr(_, cond, thenE, elseE) => {
      self->compileExpr(cond)
      self->emit(Wasm.Inst.If(Wasm.BlockReturnType.I32))
      self->compileExpr(thenE)
      self->emit(Wasm.Inst.Else)
      self->compileExpr(elseE)
      self->emit(Wasm.Inst.End)
    }

  | _ => Js.Exn.raiseError(`compileExpr: '${Core.show(expr)}' not handled`)
  }
}

let addFunc = (self: t, func: Func.t): Wasm.Func.index => {
  self.mod->Wasm.Module.addFuncMut(func->Func.toWasmFunc)
}

let compile = (expr: Core.t): Wasm.Module.t => {
  let funcs = MutableStack.make()
  let f: Func.t = {
    name: "main",
    locals: [],
    params: [],
    ret: Types.unitTy,
    instructions: [],
  }

  funcs->MutableStack.push(f)

  let self = {
    mod: Wasm.Module.make(),
    funcs: funcs,
    localCount: 0,
    scopeDepth: 0,
  }

  self->compileExpr(expr)

  self.funcs->MutableStack.forEach(f => {
    let _ = self->addFunc(f)
  })

  self.mod
}
