open Belt
open Core

module Local = {
  type t = {
    name: string,
    depth: int,
    ty: Types.monoTy,
    isMutable: bool,
  }

  let make = (name, depth, ty, ~isMutable): t => {
    name: name,
    depth: depth,
    ty: ty,
    isMutable: isMutable,
  }
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

let wasmBlockRetTyOf = (tau: Types.monoTy): Wasm.BlockReturnType.t => {
  open Types
  open Wasm.BlockReturnType
  switch tau {
  | TyConst("u32", []) => I32
  | TyConst("u64", []) => I64
  | TyConst("bool", []) => I32
  | TyConst("()", []) => I32
  | _ => Js.Exn.raiseError(`wasmBlockRetTyOf: cannot convert ${showMonoTy(tau)}`)
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
    ~isMutable: bool,
  ): Wasm.Func.Locals.index => {
    let _ = self.locals->Js.Array2.push(Local.make(name, scopeDepth, ty, ~isMutable))
    self.params->Array.length + self.locals->Array.length - 1
  }

  let findLocal = (self: t, name: string): (Wasm.Func.Locals.index, bool) => {
    switch self.params->Array.getIndexBy(((x, _)) => x == name) {
    | Some(idx) => (idx, true)
    | None =>
      switch self.locals->ArrayUtils.getReverseIndexBy(local => local.name == name) {
      | Some(idx) => (self.params->Array.length + idx, Array.getExn(self.locals, idx).isMutable)
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
    let body = Wasm.Func.Body.make(locals, self.instructions->Optimizer.peephole)

    Wasm.Func.make(sig, body)
  }
}

type t = {
  mod: Wasm.Module.t,
  funcs: array<Func.t>,
  mutable scopeDepth: int,
}

let getCurrentFuncExn = (self: t): Func.t => {
  switch self.funcs->Array.get(self.funcs->Array.length - 1) {
  | Some(f) => f
  | None => Js.Exn.raiseError(`called getFuncExn on an empty function list`)
  }
}

let findFuncIndexByName = (self: t, name: string): option<Wasm.Func.index> => {
  self.funcs->Array.getIndexBy(f => f.name == name)
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

let declareLocalVar = (self: t, name: string, ty: Types.monoTy, ~isMutable: bool): unit => {
  let f = self->getCurrentFuncExn
  let localIndex = f->Func.addLocal(name, ty, self.scopeDepth, ~isMutable)
  self->emit(Wasm.Inst.SetLocal(localIndex))
}

let resolveLocalVar = (self: t, name: string): (Wasm.Func.Locals.index, bool) => {
  let f = self->getCurrentFuncExn
  f->Func.findLocal(name)
}

let emitUnit = (self: t): unit => {
  self->emit(Wasm.Inst.ConstI32(0))
}

let rec compileExpr = (self: t, expr: CoreExpr.t): unit => {
  switch expr {
  | CoreConstExpr(_, c) => {
      let inst = switch c {
      | Ast.Expr.Const.U32Const(n) => Wasm.Inst.ConstI32(n)
      | Ast.Expr.Const.BoolConst(b) => Wasm.Inst.ConstI32(b ? 1 : 0)
      | Ast.Expr.Const.UnitConst => Wasm.Inst.ConstI32(0)
      }

      self->emit(inst)
    }
  | CoreBinOpExpr(_, lhs, op, rhs) => {
      let opInst = switch op {
      | Token.BinOp.Plus => Wasm.Inst.AddI32
      | Token.BinOp.Sub => Wasm.Inst.SubI32
      | Token.BinOp.Mult => Wasm.Inst.MulI32
      | Token.BinOp.Div => Wasm.Inst.DivI32Unsigned
      | Token.BinOp.EqEq => Wasm.Inst.EqI32
      | Token.BinOp.Neq => Wasm.Inst.NeI32
      | Token.BinOp.Lss => Wasm.Inst.LtI32Unsigned
      | Token.BinOp.Leq => Wasm.Inst.LeI32Unsigned
      | Token.BinOp.Gtr => Wasm.Inst.GtI32Unsigned
      | Token.BinOp.Geq => Wasm.Inst.GeI32Unsigned
      | Token.BinOp.Mod => Wasm.Inst.RemI32Unsigned
      }

      self->compileExpr(lhs)
      self->compileExpr(rhs)
      self->emit(opInst)
    }
  | CoreBlockExpr(_, stmts, lastExpr) => {
      self->beginScope
      stmts->Array.forEach(self->compileStmt)

      switch lastExpr {
      | Some(expr) => self->compileExpr(expr)
      | None => self->emitUnit
      }

      self->endScope
    }
  | CoreIfExpr(tau, cond, thenExpr, elseExpr) => {
      let retTy = tau->wasmBlockRetTyOf
      self->compileExpr(cond)
      self->emit(Wasm.Inst.If(retTy))
      self->compileExpr(thenExpr)
      self->emit(Wasm.Inst.Else)
      self->compileExpr(elseExpr)
      self->emit(Wasm.Inst.End)
    }
  | CoreLetInExpr(_, x, valExpr, inExpr) => {
      let x = x.contents
      self->compileExpr(valExpr)
      self->declareLocalVar(x.name, x.ty, ~isMutable=false)
      self->compileExpr(inExpr)
    }
  | CoreVarExpr(x) => {
      let (idx, _) = self->resolveLocalVar(x.contents.name)
      self->emit(Wasm.Inst.GetLocal(idx))
    }
  | CoreAssignmentExpr(x, rhs) => {
      let (idx, isMutable) = self->resolveLocalVar(x.contents.name)

      if !isMutable {
        Js.Exn.raiseError(`${x.contents.name} is immutable`)
      }

      self->compileExpr(rhs)
      self->emit(Wasm.Inst.SetLocal(idx))
      self->emitUnit
    }
  | CoreWhileExpr(cond, body) => {
      self->emit(Wasm.Inst.Block(Wasm.BlockReturnType.Void))
      self->emit(Wasm.Inst.Loop(Wasm.BlockReturnType.Void))
      self->compileExpr(cond)
      self->emit(Wasm.Inst.EqzI32)
      self->emit(Wasm.Inst.BranchIf(1))
      self->compileExpr(body)
      self->emit(Wasm.Inst.Drop)
      self->emit(Wasm.Inst.Branch(0))
      self->emit(Wasm.Inst.End)
      self->emit(Wasm.Inst.End)
      self->emitUnit
    }
  | CoreAppExpr(_, lhs, args) =>
    switch lhs {
    | CoreVarExpr(f) =>
      switch self->findFuncIndexByName(f.contents.name) {
      | Some(idx) => {
          args->Array.forEach(arg => {
            self->compileExpr(arg)
          })
          self->emit(Wasm.Inst.Call(idx))
        }
      | None => Js.Exn.raiseError(`no function named "${f.contents.name}" found`)
      }
    | _ => Js.Exn.raiseError("indirect function application not supported yet")
    }
  | CoreReturnExpr(expr) => {
      self->compileExpr(expr)
      self->emit(Wasm.Inst.Return)
    }
  | _ => Js.Exn.raiseError(`compileExpr: '${CoreExpr.show(expr)}' not handled`)
  }
}

and compileStmt = (self: t, stmt: CoreStmt.t): unit => {
  switch stmt {
  | CoreExprStmt(expr) => {
      self->compileExpr(expr)
      // drop the return value on the stack
      self->emit(Wasm.Inst.Drop)
    }
  | CoreLetStmt(x, isMutable, rhs) => {
      self->compileExpr(rhs)
      self->declareLocalVar(x.contents.name, x.contents.ty, ~isMutable)
    }
  }
}

and compileDecl = (self: t, decl: CoreDecl.t): unit => {
  switch decl {
  | CoreFuncDecl(f, args, body) => {
      let args = args->Array.map(x => (x.contents.name, x.contents.ty))
      let func = Func.make(f.contents.name, args, CoreExpr.tyVarOf(body))
      let _ = self.funcs->Js.Array2.push(func)

      self->compileExpr(body)
      self->emit(Wasm.Inst.End)
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
    let (sig, body) = f->Func.toWasmFunc
    let _ = self.mod->Wasm.Module.addExportedFunc(f.name, sig, body)
  })

  self.mod
}
