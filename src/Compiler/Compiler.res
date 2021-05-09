open Belt
open Core

module CompilerExn = {
  exception InvalidTypeConversion(Types.monoTy)
  exception InvalidFunctionSignature(Types.monoTy)
  exception VariableNotFound(string)
  exception EmptyFunctionStack
  exception CannotReassignImmutableValue(string)
  exception FunctionNotFound(string)
  exception Unimplemented(string)
  exception UnsupportedGlobalInitializer(CoreExpr.t)
  exception InvalidTypeAssertion(Types.monoTy, Types.monoTy)
  exception DerefUsedOutsideOfUnsafeBlock

  let show = exn =>
    switch exn {
    | InvalidTypeConversion(ty) => `unsupported type: ${Types.showMonoTy(ty)}`
    | InvalidFunctionSignature(ty) => `invalid function signature: ${Types.showMonoTy(ty)}`
    | VariableNotFound(x) => `variable "${x}" not found`
    | EmptyFunctionStack => "Function stack is empty"
    | CannotReassignImmutableValue(x) => `cannot reassign "${x}" as it is immutable`
    | FunctionNotFound(f) => `no function named "${f}" found`
    | Unimplemented(message) => `unimplemented: ${message}`
    | UnsupportedGlobalInitializer(expr) => `unsupported global initializer: ${CoreExpr.show(expr)}`
    | InvalidTypeAssertion(a, b) =>
      `invalid type assertion from ${Types.showMonoTy(a)} to ${Types.showMonoTy(b)}`
    | DerefUsedOutsideOfUnsafeBlock => `the deref operator can only be used in an unsafe block`
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
  | TyConst("Ptr", _) => I32
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
  | TyConst("Ptr", _) => I32
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

  let findLocal = (self: t, name: string): option<(Wasm.Func.Locals.index, bool)> => {
    switch self.params->Array.getIndexBy(((x, _)) => x == name) {
    | Some(idx) => Some((idx, true))
    | None =>
      switch self.locals->ArrayUtils.getReverseIndexBy(local => local.name == name) {
      | Some(idx) =>
        Some((self.params->Array.length + idx, Array.getExn(self.locals, idx).isMutable))
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

module Global = {
  type t = {
    name: string,
    isMutable: bool,
    ty: Wasm.ValueType.t,
    index: int,
    init: Wasm.Global.Initializer.t,
  }

  let make = (name, isMutable, ty, index, init): t => {
    name: name,
    isMutable: isMutable,
    ty: ty->wasmValueTyOf,
    index: index,
    init: init,
  }

  let toWasmGlobal = ({isMutable, ty, init}: t): Wasm.Global.t => {
    Wasm.Global.make(~isMutable, ty, init)
  }
}

type t = {
  mod: Wasm.Module.t,
  funcs: array<Func.t>,
  mutable scopeDepth: int,
  funcStack: MutableStack.t<Func.t>,
  globals: HashMap.String.t<Global.t>,
  unsafeBlockStack: MutableStack.t<bool>,
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

let isInUnsafeBlock = (self: t): bool => {
  switch self.unsafeBlockStack->MutableStack.top {
  | Some(isUnsafe) => isUnsafe
  | None => false
  }
}

let beginScope = (self: t, isUnsafe: bool): unit => {
  self.scopeDepth = self.scopeDepth + 1
  self.unsafeBlockStack->MutableStack.push(isUnsafe || self->isInUnsafeBlock)
}

let endScope = (self: t): unit => {
  self.scopeDepth = self.scopeDepth - 1
  let _ = self.unsafeBlockStack->MutableStack.pop
}

let declareLocalVar = (self: t, name: string, ty: Types.monoTy, ~isMutable: bool): unit => {
  let f = self->getCurrentFuncExn
  let localIndex = f->Func.addLocal(name, ty, self.scopeDepth, ~isMutable)
  self->emit(Wasm.Inst.SetLocal(localIndex))
}

type varInfo = Local(Wasm.Func.Locals.index, bool) | Global(Wasm.Global.index, bool)

let declareGlobal = (self: t, name, isMutable: bool, ty, init): Wasm.Global.index => {
  let index = self.globals->HashMap.String.size
  let global = Global.make(name, isMutable, ty, index, init)
  self.globals->HashMap.String.set(name, global)

  index
}

let resolveGlobal = (self: t, name: string): option<Global.t> => {
  self.globals->HashMap.String.get(name)
}

let resolveVar = (self: t, name: string): option<varInfo> => {
  let f = self->getCurrentFuncExn
  switch f->Func.findLocal(name) {
  | Some((localIndex, isMutable)) => Some(Local(localIndex, isMutable))
  | None => self->resolveGlobal(name)->Option.map(({index, isMutable}) => Global(index, isMutable))
  }
}

let emitUnit = (self: t): unit => {
  self->emit(Wasm.Inst.ConstI32(0))
}

let encodeConstExpr = c => {
  switch c {
  | Ast.Const.U32Const(n) => Wasm.Inst.ConstI32(n)
  | Ast.Const.BoolConst(b) => Wasm.Inst.ConstI32(b ? 1 : 0)
  | Ast.Const.UnitConst => Wasm.Inst.ConstI32(0)
  }
}

let ensureIsInUnsafeBlock = (self: t): unit => {
  if !(self->isInUnsafeBlock) {
    raise(CompilerExn.DerefUsedOutsideOfUnsafeBlock)
  }
}

let rec compileExpr = (self: t, expr: CoreExpr.t): unit => {
  switch expr {
  | CoreConstExpr(_, c) => self->emit(encodeConstExpr(c))
  | CoreUnaryOpExpr(_, op, expr) => {
      open Ast.UnaryOp

      let opInst = switch op {
      | Neg => raise(CompilerExn.Unimplemented("u32 negation is not handled"))
      | Not => Wasm.Inst.EqzI32
      | Deref => {
          self->ensureIsInUnsafeBlock

          Wasm.Inst.LoadI32(expr->CoreAst.typeOfExpr->Types.sizeLog2, 0)
        }
      }

      self->compileExpr(expr)
      self->emit(opInst)
    }
  | CoreBinOpExpr(_, lhs, op, rhs) => {
      open Ast.BinOp
      let opInst = switch op {
      | Plus => Wasm.Inst.AddI32
      | Sub => Wasm.Inst.SubI32
      | Mult => Wasm.Inst.MulI32
      | Div => Wasm.Inst.DivI32Unsigned
      | Equ => Wasm.Inst.EqI32
      | Neq => Wasm.Inst.NeI32
      | Lss => Wasm.Inst.LtI32Unsigned
      | Leq => Wasm.Inst.LeI32Unsigned
      | Gtr => Wasm.Inst.GtI32Unsigned
      | Geq => Wasm.Inst.GeI32Unsigned
      | Mod => Wasm.Inst.RemI32Unsigned
      }

      self->compileExpr(lhs)
      self->compileExpr(rhs)
      self->emit(opInst)
    }
  | CoreBlockExpr(_, stmts, lastExpr, safety) => {
      self->beginScope(safety == Ast.Unsafe)
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
  | CoreVarExpr(x) =>
    switch self->resolveVar(x.contents.newName) {
    | Some(Local(index, _)) => self->emit(Wasm.Inst.GetLocal(index))
    | Some(Global(index, _)) => self->emit(Wasm.Inst.GetGlobal(index))
    | None => raise(CompilerExn.VariableNotFound(x.contents.name))
    }
  | CoreAssignmentExpr(lhs, rhs) =>
    switch lhs {
    | CoreVarExpr(x) =>
      switch self->resolveVar(x.contents.newName) {
      | Some(Local(_, false)) => raise(CompilerExn.CannotReassignImmutableValue(x.contents.name))
      | Some(Global(_, false)) => raise(CompilerExn.CannotReassignImmutableValue(x.contents.name))
      | Some(var) => {
          let inst = switch var {
          | Local(index, _) => Wasm.Inst.SetLocal(index)
          | Global(index, _) => Wasm.Inst.SetGlobal(index)
          }

          self->compileExpr(rhs)
          self->emit(inst)
          self->emitUnit
        }
      | None => raise(CompilerExn.VariableNotFound(x.contents.name))
      }
    | CoreUnaryOpExpr(_, Ast.UnaryOp.Deref, expr) => {
        self->ensureIsInUnsafeBlock

        self->compileExpr(expr)
        self->compileExpr(rhs)
        self->emit(Wasm.Inst.StoreI32(rhs->CoreAst.typeOfExpr->Types.sizeLog2, 0))
        self->emitUnit
      }
    | _ =>
      raise(CompilerExn.Unimplemented("[unreachable]: assignement to an invalid lhs expression"))
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

        let funcTy = lhs->CoreAst.typeOfExpr
        let funcSig = funcTy->funcSignatureOf
        let funcSigIndex = self.mod->Wasm.Module.addSignature(funcSig)

        self->compileExpr(lhs)
        self->emit(Wasm.Inst.CallIndirect(funcSigIndex, 0))
      }

      switch lhs {
      | CoreVarExpr(f) =>
        switch self->resolveVar(f.contents.newName) {
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
  | CoreTypeAssertion(expr, originalTy, assertedTy) => {
      open Types

      switch (originalTy, assertedTy) {
      | _ if originalTy == assertedTy => ()
      | (TyConst("u32", []), TyConst("Ptr", [_ptr_ty])) => ()
      | (TyConst("Ptr", [TyConst("u32", [])]), TyConst("u32", [])) => ()
      | _ => raise(CompilerExn.InvalidTypeAssertion(originalTy, assertedTy))
      }

      self->compileExpr(expr)
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
  let func = Func.make(f.contents.newName, args, CoreExpr.typeOf(body))

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
  | CoreGlobalDecl(x, mut, init) => {
      let init = switch init {
      | CoreConstExpr(_, c) =>
        switch encodeConstExpr(c) {
        | Wasm.Inst.ConstI32(n) => Wasm.Global.Initializer.InitConstI32(n)
        | _ => raise(CompilerExn.UnsupportedGlobalInitializer(init))
        }
      | _ => raise(CompilerExn.UnsupportedGlobalInitializer(init))
      }

      let _ = self->declareGlobal(x.contents.name, mut, x.contents.ty, init)
    }
  }
}

let compile = (prog: array<CoreDecl.t>): result<Wasm.Module.t, string> => {
  let self = {
    mod: Wasm.Module.make(),
    funcs: [],
    scopeDepth: 0,
    funcStack: MutableStack.make(),
    globals: HashMap.String.make(~hintSize=1),
    unsafeBlockStack: MutableStack.make(),
  }

  try {
    prog->Array.forEach(self->compileDecl)

    // add memory
    let _ = self.mod->Wasm.Module.addMemory(Wasm.Memory.make(Wasm.Limits.make(352, None)))

    // add globals
    self.globals->HashMap.String.forEach((_, global) => {
      self.mod->Wasm.Module.addGlobal(global->Global.toWasmGlobal)
    })

    // add function references
    let funcRefs = Wasm.Element.fromFuncRefs(
      ~offset=0,
      self.funcs->Array.mapWithIndex((index, _) => index),
    )

    self.mod->Wasm.Module.addElement(funcRefs)
    self.mod->Wasm.Module.addTable(
      Wasm.Table.make(Wasm.ReferenceType.FuncRef, Wasm.Limits.makeExact(self.funcs->Array.length)),
    )

    // compile function
    self.funcs->Array.forEach(f => {
      let (sig, body) = f->Func.toWasmFunc
      let _ = self.mod->Wasm.Module.addExportedFunc(f.name, sig, body)
    })

    Ok(self.mod)
  } catch {
  | exn => Error(CompilerExn.show(exn))
  }
}
