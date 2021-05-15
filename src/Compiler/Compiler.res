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
  exception UndeclaredStruct(string)
  exception InvalidAttributeAccess(string, Types.monoTy)
  exception StructTypeNotMatched(Types.monoTy)
  exception AmibguousStruct(Types.Attributes.t, array<Context.Struct.t>)

  let show = exn =>
    switch exn {
    | InvalidTypeConversion(ty) => `unsupported type: ${Types.showMonoTy(ty)}`
    | InvalidFunctionSignature(ty) => `invalid function signature: ${Types.showMonoTy(ty)}`
    | VariableNotFound(x) => `variable "${x}" not found`
    | EmptyFunctionStack => "Function stack is empty"
    | CannotReassignImmutableValue(x) => `cannot mutate "${x}" as it is behind an immutable binding`
    | FunctionNotFound(f) => `no function named "${f}" found`
    | Unimplemented(message) => `unimplemented: ${message}`
    | UnsupportedGlobalInitializer(expr) => `unsupported global initializer: ${CoreExpr.show(expr)}`
    | InvalidTypeAssertion(a, b) =>
      `invalid type assertion from ${Types.showMonoTy(a)} to ${Types.showMonoTy(b)}`
    | DerefUsedOutsideOfUnsafeBlock => `the deref operator can only be used in an unsafe block`
    | Types.Size.UnkownTypeSize(ty) => `unknown type size: ${Types.showMonoTy(ty)}`
    | UndeclaredStruct(name) => `undeclared struct "${name}"`
    | InvalidAttributeAccess(attr, ty) =>
      `${attr} does not exist for type "${Types.showMonoTy(ty)}"`
    | StructTypeNotMatched(ty) => `No struct declaration matches type ${Types.showMonoTy(ty)}`
    | AmibguousStruct(attrs, matches) =>
      Inferencer.StructMatching.ambiguousMatchesError(attrs, matches)
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

let getStructExn = (structTy: Types.structTy) => {
  open Inferencer.StructMatching
  switch structTy {
  | NamedStruct(name) =>
    switch Context.getStruct(name) {
    | Some(s) => s
    | None => raise(CompilerExn.UndeclaredStruct(name))
    }
  | PartialStruct(attrs) =>
    switch attrs->findMatchingStruct {
    | OneMatch(s) => s
    | NoMatch => raise(CompilerExn.StructTypeNotMatched(Types.TyStruct(structTy)))
    | MultipleMatches(matches) => raise(CompilerExn.AmibguousStruct(attrs, matches))
    }
  }
}

let rec wasmValueTypeOf = (tau: Types.monoTy): array<Wasm.ValueType.t> => {
  open Types
  open Wasm.ValueType
  switch tau {
  | TyConst("u32", []) => [I32]
  | TyConst("u64", []) => [I64]
  | TyConst("bool", []) => [I32]
  | TyConst("()", []) => []
  | TyConst("Fun", _) => [I32]
  | TyConst("Ptr", _) => [I32]
  | TyConst("Tuple", tys) => tys->Array.map(wasmValueTypeOf)->Array.concatMany
  | TyStruct(structTy) => {
      let {size} = getStructExn(structTy)
      size == 0 ? [] : [I32]
    }
  | _ => raise(CompilerExn.InvalidTypeConversion(tau))
  }
}

let wasmBlockReturnTypeOf = (mod: Wasm.Module.t, tau: Types.monoTy): Wasm.BlockReturnType.t => {
  open Types
  open Wasm.BlockReturnType
  switch tau {
  | TyConst("Tuple", tys) => {
      let rets = tys->Array.map(wasmValueTypeOf)->Array.concatMany
      let signature = Wasm.Func.Signature.FuncSig([], rets)
      let idx = mod->Wasm.Module.getOrAddSignature(signature)
      TypeIndex(idx, ([], rets))
    }
  | _ =>
    switch tau->wasmValueTypeOf {
    | [] => Void
    | [ty] => TypeValue(ty)
    | _ => raise(CompilerExn.Unimplemented(`wasmBlockReturnTypeOf for ${Types.showMonoTy(tau)}`))
    }
  }
}

let funcSignatureOf = ty =>
  switch ty {
  | Types.TyConst("Fun", tys) => {
      let params = tys->Js.Array2.slice(~start=0, ~end_=tys->Array.length - 1)

      let ret = tys->Array.get(tys->Array.length - 1)

      Wasm.Func.Signature.make(
        params->Array.map(wasmValueTypeOf)->Array.concatMany,
        ret->Option.mapWithDefault([], wasmValueTypeOf),
      )
    }
  | _ => raise(CompilerExn.InvalidFunctionSignature(ty))
  }

module Func = {
  type t = {
    name: string,
    locals: array<Local.t>,
    params: array<(string, Types.monoTy, bool)>,
    ret: Types.monoTy,
    instructions: array<Wasm.Inst.t>,
  }

  let make = (name, params, ret): t => {
    let self: t = {
      name: name,
      locals: [],
      params: params->Array.keep(((_, xTy, _)) => !(xTy->Types.Size.isZeroSizedType)),
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
  ): Wasm.Func.Local.index => {
    let _ = self.locals->Js.Array2.push(Local.make(name, scopeDepth, ty, ~isMutable))
    self.params->Array.length + self.locals->Array.length - 1
  }

  let findLocal = (self: t, name: string): option<(Wasm.Func.Local.index, bool)> => {
    switch self.params->Array.getIndexBy(((x, _, _)) => x == name) {
    | Some(idx) => {
        let (_, _, mut) = self.params->Array.getExn(idx)
        Some((idx, mut))
      }
    | None =>
      switch self.locals->ArrayUtils.getReverseIndexBy(local => local.name == name) {
      | Some(idx) =>
        Some((self.params->Array.length + idx, Array.getExn(self.locals, idx).isMutable))
      | None => None
      }
    }
  }

  let toWasmFunc = (self: t): Wasm.Func.t => {
    let sig = funcSignatureOf(Types.funTy(self.params->Array.map(((_, xTy, _)) => xTy), self.ret))

    let locals = Wasm.Func.Local.fromTypes(
      self.locals
      ->Array.map(l => wasmValueTypeOf(l.ty)->Array.map(ty => (l.name, ty)))
      ->Array.concatMany,
    )

    let body = Wasm.Func.Body.make(locals, self.instructions->Optimizer.peephole)

    Wasm.Func.make(self.name, sig, body)
  }
}

module Global = {
  type t = {
    name: string,
    isMutable: bool,
    ty: option<Wasm.ValueType.t>,
    index: int,
    init: Wasm.Global.Initializer.t,
  }

  let make = (name, isMutable, ty, index, init): t => {
    name: name,
    isMutable: isMutable,
    ty: ty,
    index: index,
    init: init,
  }

  let toWasmGlobal = ({isMutable, ty, init}: t): option<Wasm.Global.t> => {
    ty->Option.map(ty => Wasm.Global.make(~isMutable, ty, init))
  }
}

type blockInfo = {
  isUnsafe: bool,
  shadowStackDelta: ref<int>,
}

type t = {
  mod: Wasm.Module.t,
  funcs: array<Func.t>,
  mutable scopeDepth: int,
  funcStack: MutableStack.t<Func.t>,
  globals: HashMap.String.t<Global.t>,
  stackTop: Global.t,
  blockStack: MutableStack.t<blockInfo>,
}

let getCurrentFuncExn = (self: t): Func.t => {
  switch self.funcStack->MutableStack.top {
  | Some(f) => f
  | None => raise(CompilerExn.EmptyFunctionStack)
  }
}

let findFuncIndexByName = (self: t, name: string): option<(Func.t, Wasm.Func.index)> => {
  self.funcs->ArrayUtils.getValueAndIndexBy(f => f.name == name)
}

let emit = (self: t, inst: Wasm.Inst.t): unit => {
  self->getCurrentFuncExn->Func.emit(inst)
}

let pushShadowStack = (self: t, bytesCount: int): unit => {
  switch self.blockStack->MutableStack.top {
  | Some({shadowStackDelta}) =>
    if bytesCount > 0 {
      shadowStackDelta.contents = shadowStackDelta.contents + bytesCount
      self->emit(Wasm.Inst.GetGlobal(self.stackTop.index))
      self->emit(Wasm.Inst.ConstI32(bytesCount))
      self->emit(Wasm.Inst.SubI32)
      self->emit(Wasm.Inst.SetGlobal(self.stackTop.index))
    }
  | None => ()
  }
}

let popShadowStack = (self: t, bytesCount: int): unit => {
  switch self.blockStack->MutableStack.top {
  | Some({shadowStackDelta}) =>
    if bytesCount > 0 {
      shadowStackDelta.contents = shadowStackDelta.contents - bytesCount
      self->emit(Wasm.Inst.GetGlobal(self.stackTop.index))
      self->emit(Wasm.Inst.ConstI32(bytesCount))
      self->emit(Wasm.Inst.AddI32)
      self->emit(Wasm.Inst.SetGlobal(self.stackTop.index))
    }
  | None => ()
  }
}

let isInUnsafeBlock = (self: t): bool => {
  switch self.blockStack->MutableStack.top {
  | Some({isUnsafe}) => isUnsafe
  | None => false
  }
}

let beginScope = (self: t, isUnsafe: bool): unit => {
  self.scopeDepth = self.scopeDepth + 1
  self.blockStack->MutableStack.push({
    isUnsafe: isUnsafe || self->isInUnsafeBlock,
    shadowStackDelta: ref(0),
  })
}

let endScope = (self: t): unit => {
  self.scopeDepth = self.scopeDepth - 1

  // restore the shadow stack
  self->popShadowStack(
    self.blockStack
    ->MutableStack.top
    ->Option.mapWithDefault(0, ({shadowStackDelta}) => shadowStackDelta.contents),
  )

  let _ = self.blockStack->MutableStack.pop
}

let declareLocalVar = (self: t, name: string, ty: Types.monoTy, ~isMutable: bool): unit => {
  let f = self->getCurrentFuncExn
  let localIndex = f->Func.addLocal(name, ty, self.scopeDepth, ~isMutable)
  self->emit(Wasm.Inst.SetLocal(localIndex))
}

type varInfo = Local(Wasm.Func.Local.index, bool) | Global(Wasm.Global.index, bool)

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

let encodeConstExpr = c => {
  switch c {
  | Ast.Const.U32Const(n) => Some(Wasm.Inst.ConstI32(n))
  | Ast.Const.BoolConst(b) => Some(Wasm.Inst.ConstI32(b ? 1 : 0))
  | Ast.Const.UnitConst => None
  }
}

let ensureIsInUnsafeBlock = (self: t): unit => {
  if !(self->isInUnsafeBlock) {
    raise(CompilerExn.DerefUsedOutsideOfUnsafeBlock)
  }
}

type structAttributeInfo = {
  offset: int,
  ty: Types.monoTy,
  size: int,
  mut: bool,
}

type attributeInfo = StructAttr(structAttributeInfo) | StructImpl((Func.t, Wasm.Func.index, bool))

let getStructAttributeOffset = (self: t, structTy: Types.monoTy, attr: string): result<
  attributeInfo,
  exn,
> => {
  switch structTy {
  | TyStruct(ty) =>
    let {attributes, name: structName} = getStructExn(ty)
    switch attributes->Array.getBy(({name}) => name == attr) {
    | Some({offset, ty, size, impl, mut}) =>
      switch impl {
      | Some((funcName, isSelfMutable)) =>
        switch self->findFuncIndexByName(
          Inferencer.renameStructImpl(structName, funcName.contents.name),
        ) {
        | Some((func, funcIdx)) => Ok(StructImpl((func, funcIdx, isSelfMutable)))
        | None => Error(CompilerExn.InvalidAttributeAccess(attr, structTy))
        }
      | None => Ok(StructAttr({offset: offset, ty: ty, size: size, mut: mut}))
      }
    | None => Error(CompilerExn.InvalidAttributeAccess(attr, structTy))
    }

  | _ => Error(CompilerExn.InvalidAttributeAccess(attr, structTy))
  }
}

let rec findImmutableBinding = (self: t, expr: CoreExpr.t): option<string> => {
  switch expr {
  | CoreVarExpr(x) =>
    switch self->resolveVar(x.contents.newName) {
    | Some(Local(_, mut)) | Some(Global(_, mut)) => mut ? None : Some(x.contents.name)
    | None => raise(CompilerExn.VariableNotFound(x.contents.name))
    }
  | CoreAttributeAccessExpr(_, lhs, attr) =>
    switch self->getStructAttributeOffset(lhs->CoreAst.typeOfExpr, attr) {
    | Ok(StructAttr({mut})) => mut ? self->findImmutableBinding(lhs) : Some(attr)
    | Ok(StructImpl(_)) => None
    | Error(err) => raise(err)
    }
  | _ => None
  }
}

let rec compileExpr = (self: t, expr: CoreExpr.t): unit => {
  switch expr {
  | CoreConstExpr(c) =>
    switch c->encodeConstExpr {
    | Some(inst) => self->emit(inst)
    | None => ()
    }
  | CoreUnaryOpExpr(_, op, expr) => {
      open Ast.UnaryOp

      let opInst = switch op {
      | Neg => raise(CompilerExn.Unimplemented("u32 negation is not handled"))
      | Not => Wasm.Inst.EqzI32
      | Deref => {
          self->ensureIsInUnsafeBlock

          Wasm.Inst.LoadI32(0, 0)
        }
      }

      self->compileExpr(expr)
      self->emit(opInst)
    }
  | CoreBinOpExpr(_, lhs, op, rhs) => {
      open Ast.BinOp

      if lhs->CoreAst.typeOfExpr->Types.Size.isZeroSizedType {
        // () == () is always true
        // () != () is always false
        // this holds for any zero-sized type
        self->compileExpr(CoreConstExpr(Ast.Const.BoolConst(op == Equ)))
      } else {
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
    }
  | CoreBlockExpr(_, stmts, lastExpr, safety) => {
      self->beginScope(safety == Ast.Unsafe)
      stmts->Array.forEach(self->compileStmt)

      switch lastExpr {
      | Some(expr) => self->compileExpr(expr)
      | None => ()
      }

      self->endScope
    }
  | CoreIfExpr(tau, cond, thenExpr, elseExpr) => {
      let retTy = self.mod->wasmBlockReturnTypeOf(tau)
      self->compileExpr(cond)
      self->emit(Wasm.Inst.If(retTy))
      self->compileExpr(thenExpr)

      if elseExpr != CoreConstExpr(Ast.Const.UnitConst) {
        self->emit(Wasm.Inst.Else)
        self->compileExpr(elseExpr)
      }

      self->emit(Wasm.Inst.End)
    }
  | CoreLetInExpr(_, x, valExpr, inExpr) => {
      let x = x.contents
      if !(valExpr->CoreAst.typeOfExpr->Types.Size.isZeroSizedType) {
        self->compileExpr(valExpr)
        self->declareLocalVar(x.name, x.ty, ~isMutable=false)
      }
      self->compileExpr(inExpr)
    }
  | CoreVarExpr(x) =>
    if x.contents.ty->Types.Size.isZeroSizedType {
      // accessing a zero sized type is a no-op
      ()
    } else {
      switch self->resolveVar(x.contents.newName) {
      | Some(Local(index, _)) => self->emit(Wasm.Inst.GetLocal(index))
      | Some(Global(index, _)) => self->emit(Wasm.Inst.GetGlobal(index))
      | None => raise(CompilerExn.VariableNotFound(x.contents.name))
      }
    }
  | CoreAssignmentExpr(lhs, rhs) =>
    if rhs->CoreAst.typeOfExpr->Types.Size.isZeroSizedType {
      ()
    } else {
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
          }
        | None => raise(CompilerExn.VariableNotFound(x.contents.name))
        }
      | CoreAttributeAccessExpr(_, lhs, attr) =>
        switch self->getStructAttributeOffset(lhs->CoreAst.typeOfExpr, attr) {
        | Ok(StructAttr({offset, size, mut})) => {
            if !mut {
              raise(CompilerExn.CannotReassignImmutableValue(attr))
            }

            switch self->findImmutableBinding(lhs) {
            | Some(name) => raise(CompilerExn.CannotReassignImmutableValue(name))
            | None => ()
            }

            if size > 0 {
              self->compileExpr(lhs)
              self->compileExpr(rhs)
              self->emit(Wasm.Inst.StoreI32(0, offset))
            }
          }
        | Ok(StructImpl(_)) =>
          raise(
            CompilerExn.Unimplemented(
              `cannot reassign ${attr} for type ${lhs->CoreAst.typeOfExpr->Types.showMonoTy}`,
            ),
          )
        | Error(err) => raise(err)
        }
      | CoreUnaryOpExpr(_, Ast.UnaryOp.Deref, expr) => {
          self->ensureIsInUnsafeBlock

          self->compileExpr(expr)
          self->compileExpr(rhs)
          self->emit(Wasm.Inst.StoreI32(rhs->CoreAst.typeOfExpr->Types.Size.sizeLog2, 0))
        }
      | _ =>
        raise(CompilerExn.Unimplemented("[unreachable]: assignement to an invalid lhs expression"))
      }
    }
  | CoreWhileExpr(cond, body) => {
      self->emit(Wasm.Inst.Block(Wasm.BlockReturnType.Void))
      self->emit(Wasm.Inst.Loop(Wasm.BlockReturnType.Void))
      self->compileExpr(cond)
      self->emit(Wasm.Inst.EqzI32)
      self->emit(Wasm.Inst.BranchIf(1))
      self->compileExpr(body)
      self->emit(Wasm.Inst.Branch(0))
      self->emit(Wasm.Inst.End)
      self->emit(Wasm.Inst.End)
    }
  | CoreAppExpr(_, lhs, args) => {
      let callIndirect = () => {
        args->Array.forEach(arg => {
          self->compileExpr(arg)
        })

        let funcTy = lhs->CoreAst.typeOfExpr
        let funcSig = funcTy->funcSignatureOf
        let funcSigIndex = self.mod->Wasm.Module.getOrAddSignature(funcSig)

        self->compileExpr(lhs)
        self->emit(Wasm.Inst.CallIndirect(funcSigIndex, 0))
      }

      let checkArgsMutability = ({params}: Func.t) => {
        args
        ->Array.zip(params)
        ->Array.forEach(((arg, (argName, _, mut))) => {
          // if the argument is mutable, make sure the given value is under a mutable binding
          if mut && self->findImmutableBinding(arg)->Option.isSome {
            raise(CompilerExn.CannotReassignImmutableValue(argName))
          }
        })
      }

      switch lhs {
      | CoreVarExpr(f) =>
        switch self->resolveVar(f.contents.newName) {
        | None =>
          switch self->findFuncIndexByName(f.contents.newName) {
          | Some((func, idx)) => {
              checkArgsMutability(func)

              args->Array.forEach(arg => {
                self->compileExpr(arg)
              })

              self->emit(Wasm.Inst.Call(idx))
            }
          | None => raise(CompilerExn.FunctionNotFound(f.contents.name))
          }
        | _ => callIndirect()
        }
      | CoreAttributeAccessExpr(_, structSelf, attr) => {
          // check args mutability
          switch self->getStructAttributeOffset(structSelf->CoreAst.typeOfExpr, attr) {
          | Ok(StructImpl((func, _, _))) => checkArgsMutability(func)
          | Ok(StructAttr(_)) => ()
          | Error(err) => raise(err)
          }

          self->compileExpr(structSelf)
          callIndirect()
        }
      | _ => callIndirect()
      }
    }
  | CoreReturnExpr(expr) => {
      switch expr {
      | Some(ret) => self->compileExpr(ret)
      | None => ()
      }

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

      let funcIndex = self->compileFuncDecl(name, args->Array.map(x => (x, false)), body)

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
  | CoreTupleExpr(exprs) =>
    exprs->Array.forEach(expr => {
      self->compileExpr(expr)
    })
  | CoreStructExpr(name, attrs) => {
      let {size: structSize, attributes} = Context.getStruct(name)->Option.getExn

      self->pushShadowStack(structSize)

      // each attribute needs the address of the begining of the struct
      attributes
      ->Array.zip(attrs)
      ->Array.forEach((({ty, mut, impl}, (_, expr))) => {
        if mut {
          switch self->findImmutableBinding(expr) {
          | Some(bindingName) => raise(CompilerExn.CannotReassignImmutableValue(bindingName))
          | _ => ()
          }
        }

        if impl->Option.isNone && ty->Types.Size.size > 0 {
          self->emit(Wasm.Inst.GetGlobal(self.stackTop.index))
        }
      })

      // the return address of the expression
      if structSize > 0 {
        self->emit(Wasm.Inst.GetGlobal(self.stackTop.index))
      }

      let offset = ref(0)

      let storeInMemory = expr => {
        let storeNbytes = (n: int) => {
          // TODO: handle alignment
          self->compileExpr(expr)
          self->emit(Wasm.Inst.StoreI32(0, offset.contents))
          offset := offset.contents + n
        }

        let ty = expr->CoreExpr.typeOf
        switch ty {
        | Types.TyConst("u32", [])
        | Types.TyConst("bool", [])
        | Types.TyConst("Fun", _)
        | Types.TyConst("Ptr", [_]) =>
          storeNbytes(4)
        | Types.TyConst("()", []) => ()
        | Types.TyStruct(structTy) => {
            let _ = getStructExn(structTy)
            storeNbytes(4)
          }
        | _ =>
          raise(
            CompilerExn.Unimplemented(`cannot store ${Types.showMonoTy(ty)} in the shadow stack`),
          )
        }
      }

      attrs->Array.forEach(((_, expr)) => {
        storeInMemory(expr)
      })
    }
  | CoreAttributeAccessExpr(_, lhs, attr) =>
    switch self->getStructAttributeOffset(lhs->CoreAst.typeOfExpr, attr) {
    | Ok(StructAttr({offset, ty: attrTy})) =>
      if !(attrTy->Types.Size.isZeroSizedType) {
        self->compileExpr(lhs)
        self->emit(Wasm.Inst.LoadI32(0, offset))
      }
    | Ok(StructImpl((_, funcIdx, isSelfMutable))) => {
        if isSelfMutable {
          switch self->findImmutableBinding(lhs) {
          | Some(immutableBinding) =>
            raise(CompilerExn.CannotReassignImmutableValue(immutableBinding))
          | None => ()
          }
        }

        self->emit(Wasm.Inst.ConstI32(funcIdx))
      }
    | Error(err) => raise(err)
    }
  }
}

and compileStmt = (self: t, stmt: CoreStmt.t): unit => {
  switch stmt {
  | CoreExprStmt(expr) => {
      self->compileExpr(expr)

      // drop the return values on the stack
      expr
      ->CoreExpr.typeOf
      ->wasmValueTypeOf
      ->Array.forEach(_ => {
        self->emit(Wasm.Inst.Drop)
      })
    }
  | CoreLetStmt(x, isMutable, rhs) =>
    self->compileExpr(rhs)

    if !(rhs->CoreAst.typeOfExpr->Types.Size.isZeroSizedType) {
      self->declareLocalVar(x.contents.name, x.contents.ty, ~isMutable)
    }
  }
}

and compileFuncDecl = (
  self: t,
  f: Context.nameRef,
  args: array<(Context.nameRef, bool)>,
  body,
): Wasm.Func.index => {
  let args = args->Array.map(((x, mut)) => (
    x.contents.name,
    x.contents.ty,
    // an argument can be reassigned in the body of a function
    // if it was marked as mutable or if it is not a referenced type
    mut || !(x.contents.ty->Types.isReferencedTy),
  ))
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
      | CoreConstExpr(c) =>
        switch encodeConstExpr(c) {
        | Some(Wasm.Inst.ConstI32(n)) => Wasm.Global.Initializer.InitConstI32(n)
        | _ => raise(CompilerExn.UnsupportedGlobalInitializer(init))
        }
      | _ => raise(CompilerExn.UnsupportedGlobalInitializer(init))
      }

      let _ = self->declareGlobal(x.contents.name, mut, Some(Wasm.ValueType.I32), init)
    }
  | CoreStructDecl(_) => () // struct declarations only affect types
  | CoreImplDecl(_, funcs) =>
    funcs->Array.forEach(((f, args, body)) => {
      self->compileDecl(CoreFuncDecl(f, args, body))
    })
  }
}

let compile = (prog: array<CoreDecl.t>): result<Wasm.Module.t, string> => {
  let self = {
    mod: Wasm.Module.make(),
    funcs: [],
    scopeDepth: 0,
    funcStack: MutableStack.make(),
    globals: HashMap.String.make(~hintSize=1),
    stackTop: Global.make(
      "__STACK_TOP__",
      true,
      Some(Wasm.ValueType.I32),
      0,
      Wasm.Global.Initializer.InitConstI32(64 * 1024 - 1),
    ),
    blockStack: MutableStack.make(),
  }

  // add the STACK_TOP global
  self.globals->HashMap.String.set(self.stackTop.name, self.stackTop)

  try {
    prog->Array.forEach(self->compileDecl)

    // add memory
    let _ = self.mod->Wasm.Module.addMemory(Wasm.Memory.make(Wasm.Limits.make(6, None)))

    // add globals
    self.globals
    ->HashMap.String.valuesToArray
    ->SortArray.stableSortBy((a, b) => a.index - b.index)
    ->Array.forEach(global => {
      switch global->Global.toWasmGlobal {
      | Some(global) => self.mod->Wasm.Module.addGlobal(global)
      | None => ()
      }
    })

    // add function references
    let funcRefs = Wasm.Elem.fromFuncRefs(
      ~offset=0,
      self.funcs->Array.mapWithIndex((index, _) => index),
    )

    self.mod->Wasm.Module.addElement(funcRefs)
    self.mod->Wasm.Module.addTable(
      Wasm.Table.make(Wasm.ReferenceType.FuncRef, Wasm.Limits.makeExact(self.funcs->Array.length)),
    )

    // compile functions
    self.funcs->Array.forEach(f => {
      let (_, sig, body) = f->Func.toWasmFunc
      let _ = self.mod->Wasm.Module.addExportedFunc(f.name, sig, body)
    })

    Ok(self.mod)
  } catch {
  | exn => Error(CompilerExn.show(exn))
  }
}
