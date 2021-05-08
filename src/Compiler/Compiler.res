open Belt
open Core

module CompilerExn = {
  exception InvalidTypeConversion(Types.monoTy)
  exception InvalidFunctionSignature(Types.monoTy)
  exception LocalNotFound(string)
  exception EmptyFunctionStack
  exception CannotReassignImmutableValue(string)
  exception FunctionNotFound(string)
  exception Unimplemented(string)

  let show = exn =>
    switch exn {
    | InvalidTypeConversion(ty) => `unsupported type: ${Types.showMonoTy(ty)}`
    | InvalidFunctionSignature(ty) => `invalid function signature: ${Types.showMonoTy(ty)}`
    | LocalNotFound(x) => `variable "${x}" not found`
    | EmptyFunctionStack => "Function stack is empty"
    | CannotReassignImmutableValue(x) => `cannot reassign "${x}" as it is immutable`
    | FunctionNotFound(f) => `no function named "${f}" found`
    | Unimplemented(message) => `unimplemented: ${message}`
    | _ => "unexpected compiler exception"
    }
}

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
  | TyConst("Fun", _) => I32
  | _ => raise(CompilerExn.InvalidTypeConversion(tau))
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
  | TyConst("Fun", _) => I32
  | _ => raise(CompilerExn.InvalidTypeConversion(tau))
  }
}

let funcSignatureOf = ty =>
  switch ty {
  | Types.TyConst("Fun", tys) => {
      let args =
        tys->Js.Array2.slice(~start=0, ~end_=tys->Array.length - 1)->Array.map(wasmValueTyOf)
      let ret = tys->Array.get(tys->Array.length - 2)->Option.map(wasmValueTyOf)

      Wasm.Func.Signature.make(args, ret)
    }
  | _ => raise(CompilerExn.InvalidFunctionSignature(ty))
  }

type localInfo = {
  index: Wasm.Func.Locals.index,
  isMutable: bool,
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

  let findLocal = (self: t, name: string): option<localInfo> => {
    switch self.params->Array.getIndexBy(((x, _)) => x == name) {
    | Some(idx) => Some({index: idx, isMutable: true})
    | None =>
      switch self.locals->ArrayUtils.getReverseIndexBy(local => local.name == name) {
      | Some(idx) =>
        Some({
          index: self.params->Array.length + idx,
          isMutable: Array.getExn(self.locals, idx).isMutable,
        })
      | None => None
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
  funcStack: MutableStack.t<Func.t>,
}

let getCurrentFuncExn = (self: t): Func.t => {
  switch self.funcStack->MutableStack.top {
  | Some(f) => f
  | None => raise(CompilerExn.EmptyFunctionStack)
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

let resolveLocalVar = (self: t, name: string): option<localInfo> => {
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
      self->declareLocalVar(x.name, x.ty, ~isMutable=false)
      self->compileExpr(valExpr)
      self->compileExpr(inExpr)
    }
  | CoreVarExpr(x) =>
    switch self->resolveLocalVar(x.contents.newName) {
    | Some({index}) => self->emit(Wasm.Inst.GetLocal(index))
    | None => raise(CompilerExn.LocalNotFound(x.contents.name))
    }
  | CoreAssignmentExpr(x, rhs) =>
    switch self->resolveLocalVar(x.contents.newName) {
    | Some({isMutable: false}) => raise(CompilerExn.CannotReassignImmutableValue(x.contents.name))
    | Some({index}) => {
        self->compileExpr(rhs)
        self->emit(Wasm.Inst.SetLocal(index))
        self->emitUnit
      }
    | None => raise(CompilerExn.LocalNotFound(x.contents.name))
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
  | CoreAppExpr(_, lhs, args) => {
      let callIndirect = () => {
        args->Array.forEach(arg => {
          self->compileExpr(arg)
        })

        let funcTy = lhs->CoreAst.tyVarOfExpr
        let funcSig = funcTy->funcSignatureOf
        let funcSigIndex = self.mod->Wasm.Module.addSignature(funcSig)

        self->compileExpr(lhs)
        self->emit(Wasm.Inst.CallIndirect(funcSigIndex, 0))
      }

      switch lhs {
      | CoreVarExpr(f) =>
        switch self->resolveLocalVar(f.contents.newName) {
        | None =>
          switch self->findFuncIndexByName(f.contents.newName) {
          | Some(idx) => {
              args->Array.forEach(arg => {
                self->compileExpr(arg)
              })
              self->emit(Wasm.Inst.Call(idx))
            }
          | None => raise(CompilerExn.FunctionNotFound(f.contents.name))
          }
        | _ => callIndirect()
        }
      | _ => callIndirect()
      }
    }
  | CoreReturnExpr(expr) => {
      self->compileExpr(expr)
      self->emit(Wasm.Inst.Return)
    }
  | CoreFuncExpr(_, originalName, args, body) => {
      let name = Context.freshIdentifier(
        "__lambda" ++ Int.toString(self.funcs->Array.length) ++ "__",
      )

      switch originalName {
      | Some(x) =>
        // renaming ${x.contents.name} to ${name.contents.name}
        x.contents.newName = name.contents.name
      | None => ()
      }

      let funcIndex = self->compileFuncDecl(name, args, body)

      self->emit(Wasm.Inst.ConstI32(funcIndex))
    }
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

and compileFuncDecl = (
  self: t,
  f: Context.nameRef,
  args: array<Context.nameRef>,
  body,
): Wasm.Func.index => {
  let args = args->Array.map(x => (x.contents.name, x.contents.ty))
  let func = Func.make(f.contents.newName, args, CoreExpr.tyVarOf(body))

  let funcIdx = self.funcs->Js.Array2.push(func) - 1
  let _ = self.funcStack->MutableStack.push(func)

  self->compileExpr(body)
  self->emit(Wasm.Inst.End)

  let _ = self.funcStack->MutableStack.pop

  funcIdx
}

and compileDecl = (self: t, decl: CoreDecl.t): unit => {
  switch decl {
  | CoreFuncDecl(f, args, body) => {
      let _ = self->compileFuncDecl(f, args, body)
    }
  }
}

let compile = (prog: array<CoreDecl.t>): result<Wasm.Module.t, string> => {
  let self = {
    mod: Wasm.Module.make(),
    funcs: [],
    scopeDepth: 0,
    funcStack: MutableStack.make(),
  }

  try {
    prog->Array.forEach(self->compileDecl)

    let funcRefs = Wasm.Element.fromFuncRefs(
      ~offset=0,
      self.funcs->Array.mapWithIndex((index, _) => index),
    )

    // add function references
    self.mod->Wasm.Module.addElement(funcRefs)
    self.mod->Wasm.Module.addTable(
      Wasm.Table.make(Wasm.ReferenceType.FuncRef, Wasm.Limits.makeExact(self.funcs->Array.length)),
    )

    self.funcs->Array.forEach(f => {
      let (sig, body) = f->Func.toWasmFunc
      let _ = self.mod->Wasm.Module.addExportedFunc(f.name, sig, body)
    })

    Ok(self.mod)
  } catch {
  | exn => Error(CompilerExn.show(exn))
  }
}
