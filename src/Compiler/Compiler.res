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

let getStructExn = (structTy: Types.structTy) => {
  open StructMatching
  switch structTy {
  | NamedStruct(name) =>
    switch Context.getStruct(name) {
    | Some(s) => s
    | None => raise(Struct.UndeclaredStruct(name))
    }
  | PartialStruct(attrs) =>
    switch attrs->findMatchingStruct {
    | OneMatch(s) => s
    | NoMatch => raise(Struct.StructTypeNotMatched(Types.TyStruct(structTy)))
    | MultipleMatches(matches) => raise(Struct.AmibguousStruct(attrs, matches))
    }
  }
}

let rec wasmValueTypeOf = (tau: Types.monoTy): array<Wasm.ValueType.t> => {
  open Types
  open Wasm.ValueType
  switch tau {
  | TyConst("u8", []) => [I32]
  | TyConst("u32", []) => [I32]
  | TyConst("u64", []) => [I64]
  | TyConst("bool", []) => [I32]
  | TyConst("char", []) => [I32]
  | TyConst("()", []) => []
  | TyConst("Fun", _) => [I32]
  | TyConst("Ptr", _) => [I32]
  | TyConst("Tuple", tys) => tys->Array.map(wasmValueTypeOf)->Array.concatMany
  | TyConst("Array", [_, TyConst(_, [])]) => [I32]
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
      switch self.locals->Utils.Array.getReverseIndexBy(local => local.name == name) {
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
  // when an initializer is not a constant expression, the global
  // is mutated once before the main function runs
  type globalMutability = InternallyMutable | Mutable | Immutable

  type t = {
    name: string,
    mutability: globalMutability,
    ty: option<Wasm.ValueType.t>,
    index: int,
    init: Wasm.Global.Initializer.t,
  }

  let make = (name, mutability, ty, index, init): t => {
    name: name,
    mutability: mutability,
    ty: ty,
    index: index,
    init: init,
  }

  let isMutable = (mutability: globalMutability): bool => {
    switch mutability {
    | InternallyMutable => false
    | Immutable => false
    | Mutable => true
    }
  }

  let toWasmGlobal = ({mutability, ty, init}: t): option<Wasm.Global.t> => {
    ty->Option.map(ty => Wasm.Global.make(~isMutable=mutability != Immutable, ty, init))
  }
}

type blockInfo = {
  isUnsafe: bool,
  shadowStackDelta: ref<int>,
}

type externFuncInfo = {
  name: string,
  args: array<(string, Types.monoTy, bool)>,
  ret: Types.monoTy,
}

type compilerOptions = {useBulkMemoryInstructions: bool}

type t = {
  mod: Wasm.Module.t,
  funcs: array<Func.t>,
  mutable scopeDepth: int,
  funcStack: MutableStack.t<Func.t>,
  globals: HashMap.String.t<Global.t>,
  globalsInitializer: Func.t,
  stackTop: Global.t,
  blockStack: MutableStack.t<blockInfo>,
  externFuncs: array<externFuncInfo>,
  options: compilerOptions,
}

let getCurrentFuncExn = (self: t): Func.t => {
  switch self.funcStack->MutableStack.top {
  | Some(f) => f
  | None => raise(CompilerExn.EmptyFunctionStack)
  }
}

let findFuncIndexByName = (self: t, name: string): option<(Func.t, Wasm.Func.index)> => {
  self.funcs
  ->Utils.Array.getValueAndIndexBy(f => f.name == name)
  ->Option.map(((f, index)) => {
    let importsCount = self.externFuncs->Array.length
    (f, importsCount + index)
  })
}

let findExternalFuncIndexByName = (self: t, name: string): option<(
  externFuncInfo,
  Wasm.Func.index,
)> => {
  self.externFuncs->Utils.Array.getValueAndIndexBy(f => f.name == name)
}

let emit = (self: t, inst: Wasm.Inst.t): unit => {
  self->getCurrentFuncExn->Func.emit(inst)
}

let rec shadowStackSizeOf = (ty: Types.monoTy) =>
  switch ty {
  | TyStruct(struct) =>
    switch struct {
    | NamedStruct(name) =>
      switch Context.getStruct(name) {
      | Some({size}) => size
      | None => raise(Struct.UndeclaredStruct(name))
      }
    | PartialStruct(_) => raise(Struct.StructTypeNotMatched(ty))
    }
  | TyConst("Tuple", tys) => tys->Array.map(shadowStackSizeOf)->Array.reduce(0, (p, c) => p + c)
  | TyConst("Array", [elemTy, TyConst(len, [])]) =>
    Int.fromString(len)->Option.getUnsafe * elemTy->Types.Size.size
  | _ => 0
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

let declareLocalVar = (
  self: t,
  name: string,
  ty: Types.monoTy,
  ~isMutable: bool,
): Wasm.Func.Local.index => {
  let f = self->getCurrentFuncExn
  let localIndex = f->Func.addLocal(name, ty, self.scopeDepth, ~isMutable)
  self->emit(Wasm.Inst.SetLocal(localIndex))
  localIndex
}

type varInfo = Local(Wasm.Func.Local.index, bool) | Global(Wasm.Global.index, bool)

let declareGlobal = (self: t, name, mutability, ty, init): Wasm.Global.index => {
  let index = self.globals->HashMap.String.size
  let global = Global.make(name, mutability, ty, index, init)
  self.globals->HashMap.String.set(name, global)

  index
}

let resolveGlobal = (self: t, name: string): option<Global.t> => {
  self.globals->HashMap.String.get(name)
}

type localInfo = {
  index: Wasm.Func.Local.index,
  isMutable: bool,
}

let resolveLocal = (self: t, name: string): option<localInfo> => {
  let f = self->getCurrentFuncExn
  f->Func.findLocal(name)->Option.map(((index, mut)) => {index: index, isMutable: mut})
}

let resolveVar = (self: t, name: string): option<varInfo> => {
  switch self->resolveLocal(name) {
  | Some({index, isMutable}) => Some(Local(index, isMutable))
  | None =>
    self
    ->resolveGlobal(name)
    ->Option.map(({index, mutability}) => Global(index, mutability->Global.isMutable))
  }
}

let encodeConstExpr = c => {
  switch c {
  | Ast.Const.U8Const(n) => Some(Wasm.Inst.ConstI32(land(n, 0xff)))
  | Ast.Const.U32Const(n) => Some(Wasm.Inst.ConstI32(land(n, 0xffffffff)))
  | Ast.Const.CharConst(c) => Some(Wasm.Inst.ConstI32(Char.code(c)))
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

let freshLambdaName = (self: t): Name.nameRef => {
  Context.freshIdentifier("$lambda" ++ Int.toString(self.funcs->Array.length))
}

let freshLocalName = (~prefix: string="", self: t): string => {
  let f = self->getCurrentFuncExn
  `$${prefix}_local` ++ Int.toString(f.locals->Array.length)
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
        let emitBinOp = opInst => {
          self->compileExpr(lhs)
          self->compileExpr(rhs)
          self->emit(opInst)
        }

        switch op {
        | Plus => emitBinOp(Wasm.Inst.AddI32)
        | Sub => emitBinOp(Wasm.Inst.SubI32)
        | Mult => emitBinOp(Wasm.Inst.MulI32)
        | Div => emitBinOp(Wasm.Inst.DivI32Unsigned)
        | Equ => emitBinOp(Wasm.Inst.EqI32)
        | Neq => emitBinOp(Wasm.Inst.NeI32)
        | Lss => emitBinOp(Wasm.Inst.LtI32Unsigned)
        | Leq => emitBinOp(Wasm.Inst.LeI32Unsigned)
        | Gtr => emitBinOp(Wasm.Inst.GtI32Unsigned)
        | Geq => emitBinOp(Wasm.Inst.GeI32Unsigned)
        | Mod => emitBinOp(Wasm.Inst.RemI32Unsigned)
        | BitwiseAnd => emitBinOp(Wasm.Inst.AndI32)
        | BitwiseOr => emitBinOp(Wasm.Inst.OrI32)
        | ShiftLeft => emitBinOp(Wasm.Inst.ShlI32)
        | ShiftRight => emitBinOp(Wasm.Inst.ShrI32Unsigned)
        | LogicalAnd =>
          self->compileExpr(
            CoreIfExpr(Types.boolTy, lhs, rhs, CoreConstExpr(Ast.Const.BoolConst(false))),
          )
        | LogicalOr =>
          self->compileExpr(
            CoreIfExpr(Types.boolTy, lhs, CoreConstExpr(Ast.Const.BoolConst(true)), rhs),
          )
        }
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
  | CoreLetInExpr(_, mut, x, valExpr, inExpr) => {
      let x = x.contents
      if !(valExpr->CoreAst.typeOfExpr->Types.Size.isZeroSizedType) {
        self->compileExpr(valExpr)
        let _ = self->declareLocalVar(x.name, x.ty, ~isMutable=mut)
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
      let callDirect = (funcIndex: int) => {
        args->Array.forEach(arg => {
          self->compileExpr(arg)
        })

        self->emit(Wasm.Inst.Call(funcIndex))
      }

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

      let checkArgsMutability = (
        params: array<(string, Types.monoTy, bool)>,
        selfArg: option<CoreExpr.t>,
      ) => {
        let args = switch selfArg {
        | Some(self) => Array.concat([self], args)
        | _ => args
        }

        args
        ->Array.zip(params)
        ->Array.forEach(((arg, (argName, ty, mut))) => {
          // if the argument is mutable reference, make sure the given value is under a mutable binding
          if mut && ty->Types.isReferencedTy && self->findImmutableBinding(arg)->Option.isSome {
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
              checkArgsMutability(func.params, None)
              callDirect(idx)
            }
          | None =>
            switch self->findExternalFuncIndexByName(f.contents.newName) {
            | Some(func, idx) => {
                checkArgsMutability(func.args, None)
                callDirect(idx)
              }
            | None => raise(CompilerExn.FunctionNotFound(f.contents.name))
            }
          }
        | _ => callIndirect()
        }
      | CoreAttributeAccessExpr(_, lhs, attr) => {
          let (isMethod, func, funcIndex) = switch lhs {
          | CoreVarExpr(structName) => {
              let structName = structName.contents.name
              switch Context.getStruct(structName) {
              | Some(_) =>
                switch self->findFuncIndexByName(Inferencer.renameStructImpl(structName, attr)) {
                | Some((func, funcIndex)) => (false, Some(func), Some(funcIndex))
                | _ =>
                  raise(
                    CompilerExn.Unimplemented(
                      `static function "${attr}" does not exists on struct ${structName}`,
                    ),
                  )
                }
              | _ => (true, None, None)
              }
            }
          | _ => (true, None, None)
          }

          let func = switch func {
          | Some(func) => Some(func)
          | None =>
            switch self->getStructAttributeOffset(lhs->CoreAst.typeOfExpr, attr) {
            | Ok(StructImpl((func, _, _))) => Some(func)
            | Ok(StructAttr(_)) => None
            | Error(err) => raise(err)
            }
          }

          // check args mutability
          func->Option.mapWithDefault((), func => {
            let selfArg = if isMethod {
              Some(lhs)
            } else {
              None
            }
            checkArgsMutability(func.params, selfArg)
          })

          // if this is a method, add self as the first argument
          if isMethod {
            self->compileExpr(lhs)
          }

          switch funcIndex {
          | Some(idx) => callDirect(idx)
          | None => callIndirect()
          }
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
  | CoreFuncExpr(_, args, body) => {
      let name = self->freshLambdaName

      let funcIndex = self->compileFuncDecl(name, args->Array.map(x => (x, false)), body)

      self->emit(Wasm.Inst.ConstI32(funcIndex))
    }
  | CoreTypeAssertionExpr(expr, originalTy, assertedTy) => {
      open Types

      switch (originalTy, assertedTy) {
      | _ if originalTy == assertedTy => ()
      | (TyConst("u32", []), TyConst("u8", [])) => ()
      | (TyConst("u32", []), TyConst("Ptr", [_ptr_ty])) => ()
      | (TyConst("Ptr", [TyConst("u32", [])]), TyConst("u32", [])) => ()

      | (TyConst("Array", [arrTy, _]), TyConst("Ptr", [ptrTy]))
        if Unification.unifiable(arrTy, ptrTy) => ()
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
        if mut && ty->Types.isReferencedTy {
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

      attrs->Array.forEach(((_, expr)) => {
        let bytes = self->storeInShadowStack(expr, offset.contents)
        offset.contents = offset.contents + bytes
      })
    }
  | CoreArrayExpr(tau, init) => {
      let arraySize = shadowStackSizeOf(tau)
      self->pushShadowStack(arraySize)
      self->emit(Wasm.Inst.GetGlobal(self.stackTop.index))
      let arrayAddrLocalIndex =
        self->declareLocalVar(
          self->freshLocalName(~prefix="arrayAddr"),
          Types.u32Ty,
          ~isMutable=true,
        )

      switch init {
      | CoreAst.ArrayInitRepeat(expr, _) => {
          self->emit(Wasm.Inst.ConstI32(0))
          let addrLocalIndex =
            self->declareLocalVar(
              self->freshLocalName(~prefix="addr"),
              Types.u32Ty,
              ~isMutable=true,
            )

          self->emit(Wasm.Inst.Block(Wasm.BlockReturnType.Void))
          self->emit(Wasm.Inst.Loop(Wasm.BlockReturnType.Void))
          self->emit(Wasm.Inst.GetLocal(addrLocalIndex))
          self->emit(Wasm.Inst.ConstI32(arraySize))
          self->emit(Wasm.Inst.EqI32)
          self->emit(Wasm.Inst.BranchIf(1))
          self->emit(Wasm.Inst.GetLocal(arrayAddrLocalIndex))
          self->emit(Wasm.Inst.GetLocal(addrLocalIndex))
          self->emit(Wasm.Inst.AddI32)
          let elemSize = self->storeInShadowStack(expr, 0)
          self->emit(Wasm.Inst.GetLocal(addrLocalIndex))
          self->emit(Wasm.Inst.ConstI32(elemSize))
          self->emit(Wasm.Inst.AddI32)
          self->emit(Wasm.Inst.SetLocal(addrLocalIndex))
          self->emit(Wasm.Inst.Branch(0))
          self->emit(Wasm.Inst.End)
          self->emit(Wasm.Inst.End)
        }
      | CoreAst.ArrayInitList(elems) => {
          let offset = ref(0)
          elems->Array.forEach(expr => {
            self->emit(Wasm.Inst.GetLocal(arrayAddrLocalIndex))
            let elemSize = self->storeInShadowStack(expr, offset.contents)
            offset.contents = offset.contents + elemSize
          })
        }
      }

      // pointer to the start of the array
      self->emit(Wasm.Inst.GetLocal(arrayAddrLocalIndex))
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
  | CoreLetRecInExpr(_, f, x, body, inExpr) => {
      let newName = self->freshLambdaName

      f.contents.newName = newName.contents.name

      let _ = self->compileFuncDecl(newName, x->Array.map(x => (x, false)), body)

      self->compileExpr(inExpr)
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
  }
}

and compileFuncDecl = (
  self: t,
  f: Name.nameRef,
  args: array<(Name.nameRef, bool)>,
  body,
): Wasm.Func.index => {
  let args = args->Array.map(((x, mut)) => {
    (
      x.contents.name,
      x.contents.ty,
      // an argument can be reassigned in the body of a function
      // if it was marked as mutable or if it is not a referenced type
      mut || !(x.contents.ty->Types.isReferencedTy),
    )
  })
  let func = Func.make(f.contents.newName, args, CoreExpr.typeOf(body))

  let funcIdx = self.funcs->Js.Array2.push(func) - 1
  let _ = self.funcStack->MutableStack.push(func)

  self->compileExpr(body)
  self->emit(Wasm.Inst.End)

  let _ = self.funcStack->MutableStack.pop

  funcIdx
}

and compileExprIsolated = (self: t, expr: Core.CoreExpr.t): Func.t => {
  let topMostFunc = Func.make("__isolated__", [], expr->CoreAst.typeOfExpr)
  let _ = self.funcStack->MutableStack.push(topMostFunc)
  self->compileExpr(expr)
  let _ = self.funcStack->MutableStack.pop
  topMostFunc
}

and compileDecl = (self: t, decl: CoreDecl.t): unit => {
  switch decl {
  | CoreFuncDecl(f, args, body) => {
      let _ = self->compileFuncDecl(f, args, body)
    }
  | CoreGlobalDecl(x, mut, init) => {
      // never release the allocated stack bytes since we are in the global scope
      self->beginScope(false)
      let {instructions} = self->compileExprIsolated(init)

      switch instructions {
      | [Wasm.Inst.ConstI32(n)] => {
          let _ =
            self->declareGlobal(
              x.contents.name,
              mut ? Global.Mutable : Global.Immutable,
              Some(Wasm.ValueType.I32),
              Wasm.Global.Initializer.fromInstructions([Wasm.Inst.ConstI32(n)]),
            )
        }
      | _ => {
          let _ = self.globalsInitializer.instructions->Js.Array2.pushMany(instructions)

          let globalIndex =
            self->declareGlobal(
              x.contents.name,
              mut ? Global.Mutable : Global.InternallyMutable,
              Some(Wasm.ValueType.I32),
              Wasm.Global.Initializer.fromInstructions([Wasm.Inst.ConstI32(0)]),
            )

          let _ =
            self.globalsInitializer.instructions->Js.Array2.push(Wasm.Inst.SetGlobal(globalIndex))
        }
      }
    }
  | CoreStructDecl(_) => () // struct declarations only affect types
  | CoreImplDecl(_, funcs) =>
    funcs->Array.forEach(((f, args, body)) => {
      self->compileDecl(CoreFuncDecl(f, args, body))
    })
  | CoreExternFuncDecl({name: f, args, ret}) => {
      let signature = f.contents.ty->funcSignatureOf
      let fSigIdx = self.mod->Wasm.Module.getOrAddSignature(signature)
      let _ =
        self.mod->Wasm.Module.addImport(
          Wasm.ImportEntry.make(
            "index",
            f.contents.name,
            Wasm.ImportEntry.ImportDesc.Func(fSigIdx),
          ),
        )

      let _ = self.externFuncs->Js.Array2.push({
        name: f.contents.name,
        args: args->Array.map(((x, mut)) => (x.contents.name, x.contents.ty, mut)),
        ret: ret,
      })
    }
  }
}

and storeInShadowStack = (self: t, expr, offset: int): int => {
  let storeNbytes = (n: int) => {
    // TODO: handle alignment
    self->compileExpr(expr)
    self->emit(Wasm.Inst.StoreI32(0, offset))
    n
  }

  let ty = expr->CoreExpr.typeOf
  switch ty {
  | Types.TyConst("u8", []) => storeNbytes(1)
  | Types.TyConst("char", []) => storeNbytes(1)
  | Types.TyConst("u32", []) => storeNbytes(4)
  | Types.TyConst("bool", []) => storeNbytes(4)
  | Types.TyConst("Fun", _) => storeNbytes(4)
  | Types.TyConst("Ptr", [_]) => storeNbytes(4)
  | Types.TyConst("()", []) => 0
  | Types.TyStruct(structTy) => {
      let _ = getStructExn(structTy)
      storeNbytes(4)
    }
  | _ =>
    raise(CompilerExn.Unimplemented(`cannot store ${Types.showMonoTy(ty)} in the shadow stack`))
  }
}

// get the minimum size needed to store structs and arrays in the shadow stack
let minShadowStackSize = (prog: array<CoreDecl.t>): int => {
  let blockDeltas = MutableStack.make()

  let onEnterBlock = _ => {
    blockDeltas->MutableStack.push(ref(0))
  }

  let visitExpr = (expr: CoreExpr.t) => {
    switch expr {
    | CoreAst.CoreStructExpr(_) | CoreAst.CoreArrayExpr(_) => {
        let lhsSize = shadowStackSizeOf(expr->CoreAst.typeOfExpr)
        switch blockDeltas->MutableStack.top {
        | Some(delta) => delta.contents = delta.contents + lhsSize
        | None => blockDeltas->MutableStack.push(ref(lhsSize))
        }
      }
    | _ => ()
    }
  }

  let onExitBlock = _ => {
    switch blockDeltas->MutableStack.top {
    | Some(delta) => blockDeltas->MutableStack.push(ref(-delta.contents))
    | None => ()
    }
  }

  CoreAst.visitProg(
    visitExpr,
    prog,
    ~onEnterBlock=Some(onEnterBlock),
    ~onExitBlock=Some(onExitBlock),
  )

  let maxStackSize = ref(0)
  let currentStackSize = ref(0)

  blockDeltas->MutableStack.forEach(delta => {
    let delta = delta.contents
    currentStackSize.contents = currentStackSize.contents + delta
    maxStackSize.contents = max(maxStackSize.contents, currentStackSize.contents)
  })

  maxStackSize.contents
}

let compile = (prog: array<CoreDecl.t>): result<Wasm.Module.t, string> => {
  let shadowStackSize = minShadowStackSize(prog)
  let self = {
    mod: Wasm.Module.make(),
    funcs: [],
    scopeDepth: 0,
    funcStack: MutableStack.make(),
    globals: HashMap.String.make(~hintSize=1),
    // wasm globals can only be constant expressions
    // so if a global initializer is more complex, add the initialization code
    // to the globalsInitializer function which gets called in the main function
    globalsInitializer: Func.make("__initGlobals__", [], Types.unitTy),
    stackTop: Global.make(
      "__STACK_TOP__",
      Global.Mutable,
      Some(Wasm.ValueType.I32),
      0,
      Wasm.Global.Initializer.fromInstructions([Wasm.Inst.ConstI32(shadowStackSize)]),
    ),
    blockStack: MutableStack.make(),
    externFuncs: [],
    options: {
      useBulkMemoryInstructions: false,
    },
  }

  // add the STACK_TOP global
  self.globals->HashMap.String.set(self.stackTop.name, self.stackTop)

  try {
    prog->Array.forEach(self->compileDecl)

    // add memory
    let wasmPageSize = 64 * 1024
    let minPagesCount = Int.fromFloat(ceil(float(shadowStackSize) /. float(wasmPageSize))) + 16
    let _ = self.mod->Wasm.Module.addMemory(Wasm.Memory.make(Wasm.Limits.make(minPagesCount, None)))

    // export memory
    self.mod->Wasm.Module.addExport(
      Wasm.ExportEntry.make("memory", Wasm.ExportEntry.ExternalKind.Memory, 0),
    )

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

    let globalsInitializerFuncIndex = self.funcs->Array.length
    let isGlobalInitializerEmpty = self.globalsInitializer.instructions->Array.length == 0

    if !isGlobalInitializerEmpty {
      let _ = self.globalsInitializer.instructions->Js.Array2.push(Wasm.Inst.End)
      let _ = self.funcs->Js.Array2.push(self.globalsInitializer)
    }

    // compile functions
    self.funcs->Array.forEach(f => {
      if f.name == "main" && !isGlobalInitializerEmpty {
        // call the globals initializer before any other code
        let _ = f.instructions->Js.Array2.unshift(Wasm.Inst.Call(globalsInitializerFuncIndex))
      }

      let (_, sig, body) = f->Func.toWasmFunc
      let _ = self.mod->Wasm.Module.addExportedFunc(f.name, sig, body)
    })

    Ok(self.mod)
  } catch {
  | exn => Error(CompilerExn.show(exn))
  }
}
