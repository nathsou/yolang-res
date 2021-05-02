open Belt
open Core

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
    params: array<(string, Types.monoTy)>,
    ret: Types.monoTy,
    instructions: array<Wasm.Inst.t>,
  }

  let make = (name, params, ret): t => {
    let self: t = {
      name: name,
      locals: [],
      params: params,
      ret: ret,
      instructions: [],
    }

    self
  }

  let emit = (self: t, inst: Wasm.Inst.t): unit => {
    let _ = self.instructions->Js.Array2.push(inst)
  }

  let addLocal = (
    self: t,
    name: string,
    ty: Types.monoTy,
    scopeDepth: int,
  ): Wasm.Func.Locals.index => {
    let _ = self.locals->Js.Array2.push(Local.make(name, scopeDepth, ty))
    self.params->Array.length + self.locals->Array.length - 1
  }

  let findLocal = (self: t, name: string): Wasm.Func.Locals.index => {
    switch self.params->Array.getIndexBy(((x, _)) => x == name) {
    | Some(idx) => idx
    | None =>
      switch self.locals->Array.getIndexBy(local => local.name == name) {
      | Some(idx) => self.params->Array.length + idx
      | None => Js.Exn.raiseError(`findLocal: ${name} not found`)
      }
    }
  }

  let toWasmFunc = (self: t): Wasm.Func.t => {
    let sig = Wasm.Func.Signature.make(
      self.params->Array.map(((_, xTy)) => xTy->wasmValueTyOf),
      Some(self.ret->wasmValueTyOf),
    )
    let locals = Wasm.Func.Locals.fromTypes(self.locals->Array.map(l => wasmValueTyOf(l.ty)))
    let body = Wasm.Func.Body.make(locals, self.instructions)

    Wasm.Func.make(sig, body)
  }
}

type t = {
  mod: Wasm.Module.t,
  funcs: array<Func.t>,
  mutable scopeDepth: int,
}

let getCurrentFuncExn = (self: t) => {
  switch self.funcs->Array.get(self.funcs->Array.length - 1) {
  | Some(f) => f
  | None => Js.Exn.raiseError(`called getFuncExn on an empty function list`)
  }
}

let emit = (self: t, inst: Wasm.Inst.t): unit => {
  self->getCurrentFuncExn->Func.emit(inst)
}

let beginScope = (self: t) => {
  self.scopeDepth = self.scopeDepth + 1
}

let endScope = (self: t) => {
  self.scopeDepth = self.scopeDepth - 1
}

let declareLocalVar = (self: t, name: string, ty: Types.monoTy): unit => {
  let f = self->getCurrentFuncExn
  let localIndex = f->Func.addLocal(name, ty, self.scopeDepth)
  self->emit(Wasm.Inst.SetLocal(localIndex))
}

let resolveLocalVar = (self: t, name: string): Wasm.Func.Locals.index => {
  let f = self->getCurrentFuncExn
  f->Func.findLocal(name)
}

let rec compileExpr = (self: t, expr: CoreExpr.t): unit => {
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
      | (CoreExpr.CoreConstExpr(_, Expr.Const.IntConst(0)), _) => {
          self->compileExpr(rhs)
          self->emit(Wasm.Inst.EqzI32)
        }
      | (_, CoreExpr.CoreConstExpr(_, Expr.Const.IntConst(0))) => {
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
  | CoreBlockExpr(_, exprs) => {
      self->beginScope
      exprs->Js.Array2.forEach(self->compileExpr)
      self->endScope
    }
  | CoreIfExpr(_, cond, thenExpr, elseExpr) => {
      self->compileExpr(cond)
      self->emit(Wasm.Inst.If(Wasm.BlockReturnType.I32))
      self->compileExpr(thenExpr)
      self->emit(Wasm.Inst.Else)
      self->compileExpr(elseExpr)
      self->emit(Wasm.Inst.End)
    }
  | CoreLetInExpr(_, (x, xTy), valExpr, inExpr) => {
      self->compileExpr(valExpr)
      self->declareLocalVar(x, xTy)
      self->compileExpr(inExpr)
    }
  | CoreVarExpr(_, x) => self->emit(Wasm.Inst.GetLocal(self->resolveLocalVar(x)))
  | _ => Js.Exn.raiseError(`compileExpr: '${CoreExpr.show(expr)}' not handled`)
  }
}

let compileDecl = (self: t, decl: CoreDecl.t) => {
  open CoreDecl

  switch decl {
  | CoreLetDecl((x, xTy), val) => {
      self->compileExpr(val)
      let _ = self->declareLocalVar(x, xTy)
    }
  | CoreFuncDecl((f, _), args, body) => {
      let func = Func.make(f, args, CoreExpr.tyVarOf(body))
      let _ = self.funcs->Js.Array2.push(func)

      self->compileExpr(body)
    }
  }
}

let compile = (prog: array<CoreDecl.t>): Wasm.Module.t => {
  let self = {
    mod: Wasm.Module.make(),
    funcs: [],
    scopeDepth: 0,
  }

  prog->Array.forEach(self->compileDecl)

  self.funcs->Array.forEach(f => {
    let _ = self.mod->Wasm.Module.addFuncMut(f->Func.toWasmFunc)
  })

  self.mod
}
