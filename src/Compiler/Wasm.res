open Belt

@genType
type byte = int

// https://www.wikiwand.com/en/LEB128
@genType
let uleb128 = (n: int): array<byte> => {
  let bytes = []

  let rec aux = val => {
    let byte = val->land(0x7f)
    let val = val->lsr(7)

    if val != 0 {
      let _ = bytes->Js.Array2.push(byte->lor(0x80))
      aux(val)
    } else {
      let _ = bytes->Js.Array2.push(byte)
    }
  }

  aux(n)
  bytes
}

@genType
let sleb128 = (val: int): array<byte> => {
  let bytes = []

  let rec aux = val => {
    let byte = val->land(0x7f)
    let val = val->asr(7)

    if (val == 0 && byte->land(0x40) == 0) || (val == -1 && byte->land(0x40) != 0) {
      let _ = bytes->Js.Array2.push(byte)
    } else {
      let _ = bytes->Js.Array2.push(byte->lor(0x80))
      aux(val)
    }
  }

  aux(val->lor(0))
  bytes
}

module Vec = {
  @genType
  type t = array<byte>

  @genType
  let encode = (vec: t) => Array.concat(uleb128(vec->Array.length), vec)

  @genType
  let encodeMany = (vecs: array<t>): t =>
    Array.concat(uleb128(vecs->Array.length), Array.concatMany(vecs))
}

module I32 = {
  @genType
  type t = int

  @genType
  let encode = (n: t): Vec.t => sleb128(n)
}

module I64 = {
  @genType
  type t = int

  @genType
  let encode = (n: t): Vec.t => sleb128(n)
}

module F32 = {
  @genType
  type t = float

  @genType
  let encode = (x: t): Vec.t => {
    open Js.Typed_array
    let view = DataView.fromBuffer(ArrayBuffer.make(4))
    view->DataView.setFloat32LittleEndian(0, x)

    [
      view->DataView.getUint8(0),
      view->DataView.getUint8(1),
      view->DataView.getUint8(2),
      view->DataView.getUint8(3),
    ]
  }
}

module F64 = {
  @genType
  type t = float

  @genType
  let encode = (x: t): Vec.t => {
    open Js.Typed_array
    let view = DataView.fromBuffer(ArrayBuffer.make(8))
    view->DataView.setFloat64LittleEndian(0, x)

    [
      view->DataView.getUint8(0),
      view->DataView.getUint8(1),
      view->DataView.getUint8(2),
      view->DataView.getUint8(3),
      view->DataView.getUint8(4),
      view->DataView.getUint8(5),
      view->DataView.getUint8(6),
      view->DataView.getUint8(7),
    ]
  }
}

module Section = {
  @genType
  type t =
    | Custom
    | Type
    | Import
    | Function
    | Table
    | Memory
    | Global
    | Export
    | Start
    | Element
    | Code
    | Data

  @genType
  let id = (section: t) =>
    switch section {
    | Custom => 0
    | Type => 1
    | Import => 2
    | Function => 3
    | Table => 4
    | Memory => 5
    | Global => 6
    | Export => 7
    | Start => 8
    | Element => 9
    | Code => 10
    | Data => 11
    }

  @genType
  let encode = (section, bytes): Vec.t => Array.concat([section->id], Vec.encode(bytes))

  @genType
  let show = sec =>
    `[${switch sec {
      | Custom => "custom"
      | Type => "type"
      | Import => "import"
      | Function => "function"
      | Table => "table"
      | Memory => "memory"
      | Global => "global"
      | Export => "export"
      | Start => "start"
      | Element => "element"
      | Code => "code"
      | Data => "data"
      }}]`
}

module ValueType = {
  @genType
  type t = I32 | I64 | F32 | F64

  @genType
  let encode = (v: t): byte =>
    switch v {
    | I32 => 0x7f
    | I64 => 0x7e
    | F32 => 0x7d
    | F64 => 0x7c
    }

  @genType
  let show = v =>
    switch v {
    | I32 => "i32"
    | I64 => "i64"
    | F32 => "f32"
    | F64 => "f64"
    }
}

module BlockReturnType = {
  @genType
  type t = TypeValue(ValueType.t) | TypeIndex(int, (array<ValueType.t>, array<ValueType.t>)) | Void

  @genType
  let encode = (v: t): array<byte> =>
    switch v {
    | TypeValue(t) => [t->ValueType.encode]
    | TypeIndex(idx, _) => uleb128(idx)
    | Void => [0x40]
    }

  @genType
  let show = v =>
    switch v {
    | TypeValue(t) => t->ValueType.show
    | TypeIndex(idx, _) => `type ${Int.toString(idx)}`
    | Void => "void"
    }
}

// https://github.com/sunfishcode/wasm-reference-manual/blob/master/WebAssembly.md#memflags-immediate-type
@genType type alignmentHint = int
@genType type addrOffset = int

module Inst = {
  @genType
  type t =
    | ConstI32(int)
    | ConstI64(int)
    | ConstF32(float)
    | ConstF64(float)
    | If(BlockReturnType.t)
    | BranchIf(int)
    | Branch(int)
    | Block(BlockReturnType.t)
    | Loop(BlockReturnType.t)
    | Else
    | End
    | Return
    | GetGlobal(int)
    | SetGlobal(int)
    | GetLocal(int)
    | SetLocal(int)
    | TeeLocal(int)
    | Call(int)
    | CallIndirect(int, int)
    | LoadI32(alignmentHint, addrOffset)
    | StoreI32(alignmentHint, addrOffset)
    | Drop
    | AddI32
    | SubI32
    | MulI32
    | DivI32Unsigned
    | DivI32Signed
    | EqzI32
    | EqI32
    | EqI64
    | EqF32
    | EqF64
    | NeI32
    | NeI64
    | NeF32
    | NeF64
    | LeI32Signed
    | LeI32Unsigned
    | LeI64Signed
    | LeI64Unsigned
    | LeF32
    | LeF64
    | GeI32Signed
    | GeI32Unsigned
    | GeI64Signed
    | GeI64Unsigned
    | GeF32
    | GeF64
    | LtI32Signed
    | LtI32Unsigned
    | LtI64Signed
    | LtI64Unsigned
    | LtF32
    | LtF64
    | GtI32Signed
    | GtI32Unsigned
    | GtI64Signed
    | GtI64Unsigned
    | GtF32
    | GtF64
    | RemI32Signed
    | RemI32Unsigned
    | AndI32
    | OrI32
    | ShlI32
    | ShrI32Signed
    | ShrI32Unsigned
    | InitMemory(int)
    | DropData(int)

  @genType
  let info = inst =>
    switch inst {
    | ConstI32(n) => ([0x41], `i32.const ${Int.toString(n)}`)
    | ConstI64(n) => ([0x42], `i64.const ${Int.toString(n)}`)
    | ConstF32(x) => ([0x43], `i64.const ${Float.toString(x)}`)
    | ConstF64(x) => ([0x44], `i64.const ${Float.toString(x)}`)
    | If(_) => ([0x04], "if")
    | BranchIf(lvl) => ([0x0d], `br_if ${Int.toString(lvl)}`)
    | Branch(lvl) => ([0x0c], `br ${Int.toString(lvl)}`)
    | Block(_) => ([0x02], "block")
    | Loop(_) => ([0x03], "loop")
    | Else => ([0x05], "else")
    | End => ([0x0b], "end")
    | Return => ([0x0f], "return")
    | GetLocal(idx) => ([0x20], "local.get " ++ Int.toString(idx))
    | SetLocal(idx) => ([0x21], "local.set " ++ Int.toString(idx))
    | TeeLocal(idx) => ([0x22], "local.tee " ++ Int.toString(idx))
    | GetGlobal(idx) => ([0x23], "global.get " ++ Int.toString(idx))
    | SetGlobal(idx) => ([0x24], "global.set " ++ Int.toString(idx))
    | Call(funcIdx) => ([0x10], "call " ++ Int.toString(funcIdx))
    | CallIndirect(typeIdx, tableIdx) => (
        [0x11],
        `call_indirect [typeIndex=${Int.toString(typeIdx)}, tableIndex=${Int.toString(tableIdx)}]`,
      )
    | LoadI32(alignment, offset) => (
        [0x28],
        `i32.load (alignment=${Int.toString(alignment)}, offset=${Int.toString(offset)})`,
      )
    | StoreI32(alignment, offset) => (
        [0x36],
        `i32.store (alignment=${Int.toString(alignment)}, offset=${Int.toString(offset)})`,
      )
    | Drop => ([0x1a], "drop")
    | AddI32 => ([0x6a], "i32.add")
    | SubI32 => ([0x6b], "i32.sub")
    | MulI32 => ([0x6c], "i32.mul")
    | DivI32Signed => ([0x6d], "i32.div_u")
    | DivI32Unsigned => ([0x6e], "i32.div_s")
    | RemI32Signed => ([0x6f], "i32.rem_s")
    | RemI32Unsigned => ([0x70], "i32.rem_u")
    | EqzI32 => ([0x45], "i32.eqz")
    | EqI32 => ([0x46], "i32.eq")
    | EqI64 => ([0x51], "i64.eq")
    | EqF32 => ([0x5b], "f32.eq")
    | EqF64 => ([0x61], "f64.eq")
    | NeI32 => ([0x47], "i32.ne")
    | NeI64 => ([0x52], "i64.ne")
    | NeF32 => ([0x5c], "f32.ne")
    | NeF64 => ([0x62], "f64.ne")
    | LeI32Signed => ([0x4c], "i32.le_s")
    | LeI32Unsigned => ([0x4d], "i32.le_u")
    | LeI64Signed => ([0x57], "i64.le_s")
    | LeI64Unsigned => ([0x58], "i64.le_u")
    | LeF32 => ([0x5f], "f32.le")
    | LeF64 => ([0x65], "f64.le")
    | GeI32Signed => ([0x4e], "i32.ge_s")
    | GeI32Unsigned => ([0x4f], "i32.ge_u")
    | GeI64Signed => ([0x59], "i64.ge_s")
    | GeI64Unsigned => ([0x5a], "i64.ge_u")
    | GeF32 => ([0x60], "f32.ge")
    | GeF64 => ([0x66], "f64.ge")
    | LtI32Signed => ([0x48], "i32.lt_s")
    | LtI32Unsigned => ([0x49], "i32.lt_u")
    | LtI64Signed => ([0x53], "i64.lt_s")
    | LtI64Unsigned => ([0x54], "i64.lt_u")
    | LtF32 => ([0x5d], "f32.lt")
    | LtF64 => ([0x63], "f64.lt")
    | GtI32Signed => ([0x4a], "i32.gt_s")
    | GtI32Unsigned => ([0x4b], "i32.gt_u")
    | GtI64Signed => ([0x55], "i64.gt_s")
    | GtI64Unsigned => ([0x56], "i64.gt_u")
    | GtF32 => ([0x5e], "f32.gt")
    | GtF64 => ([0x64], "f64.gt")
    | AndI32 => ([0x71], "i32.and")
    | OrI32 => ([0x72], "i32.or")
    | ShlI32 => ([0x74], "i32.shl")
    | ShrI32Signed => ([0x75], "i32.shr_s")
    | ShrI32Unsigned => ([0x76], "i32.shr_u")
    | InitMemory(d) => ([0xfc, 0x08], "memory.init " ++ Int.toString(d))
    | DropData(d) => ([0xfc, 0x09], "data.drop " ++ Int.toString(d))
    }

  @genType
  let opcode = inst => {
    let (opcode, _) = inst->info
    opcode
  }

  @genType
  let identationDelta = inst =>
    switch inst {
    | If(_) => 1
    | Block(_) => 1
    | Loop(_) => 1
    | Else => -1
    | End => -1
    | _ => 0
    }

  @genType
  let show = (
    ~indentation=ref(0),
    inst,
    funcNames: array<string>,
    localNames: array<string>,
  ): string => {
    let (_, fmt) = inst->info
    let delta = inst->identationDelta
    let ident =
      Array.range(0, indentation.contents + (delta < 0 ? delta : 0))->Array.joinWith("  ", _ => "")
    indentation := indentation.contents + delta

    if inst == Else {
      indentation := indentation.contents + 1
    }

    let details = switch inst {
    | Call(funcIndex) => `<${funcNames->Array.get(funcIndex)->Option.mapWithDefault("??", x => x)}>`
    | GetLocal(localIndex) | SetLocal(localIndex) | TeeLocal(localIndex) =>
      `<${localNames->Array.get(localIndex)->Option.mapWithDefault("??", x => x)}>`
    | _ => ""
    }

    ident ++ fmt ++ " " ++ details
  }

  @genType
  let encode = inst =>
    switch inst {
    | ConstI32(n) => Array.concat(inst->opcode, I32.encode(n))
    | ConstI64(n) => Array.concat(inst->opcode, I64.encode(n))
    | ConstF32(x) => Array.concat(inst->opcode, F32.encode(x))
    | ConstF64(x) => Array.concat(inst->opcode, F64.encode(x))
    | GetLocal(n) | SetLocal(n) | TeeLocal(n) | GetGlobal(n) | SetGlobal(n) =>
      Array.concat(inst->opcode, uleb128(n))
    | If(bt) | Block(bt) | Loop(bt) => Array.concat(inst->opcode, bt->BlockReturnType.encode)
    | BranchIf(depth) | Branch(depth) => Array.concat(inst->opcode, uleb128(depth))
    | Call(funcIdx) => Array.concat(inst->opcode, uleb128(funcIdx))
    | CallIndirect(typeIdx, tableIdx) =>
      Array.concatMany([inst->opcode, uleb128(typeIdx), uleb128(tableIdx)])
    | LoadI32(alignment, offset) | StoreI32(alignment, offset) =>
      Array.concatMany([inst->opcode, uleb128(alignment), uleb128(offset)])
    | InitMemory(dataIdx) => Array.concatMany([inst->opcode, uleb128(dataIdx), [0x00]])
    | DropData(dataIdx) => Array.concat(inst->opcode, uleb128(dataIdx))
    | _ => inst->opcode
    }
}

module Func = {
  module Signature = {
    @genType
    type t = FuncSig(array<ValueType.t>, array<ValueType.t>)

    @genType
    type index = int

    // http://webassembly.github.io/spec/core/binary/types.html#function-types
    @genType
    let funcTypeCode = 0x60

    @genType
    let make = (params, rets): t => FuncSig(params, rets)

    @genType
    let empty = make([], [])

    @genType
    let encode = (FuncSig(params, ret)): Vec.t => {
      Array.concatMany([
        [funcTypeCode],
        Vec.encode(params->Array.map(ValueType.encode)),
        Vec.encode(ret->Array.map(ValueType.encode)),
      ])
    }

    @genType
    let show = (FuncSig(args, rets)) => {
      let argsFmt = args->Array.joinWith(", ", ValueType.show)
      let retsFmt = rets->Array.joinWith(", ", ValueType.show)

      switch (args, rets) {
      | ([_], [_]) => `${argsFmt} -> ${retsFmt}`
      | ([_], []) => `${argsFmt} -> ()`
      | (_, []) => `(${argsFmt}) -> ()`
      | (_, [_]) => `(${argsFmt}) -> ${retsFmt}`
      | _ => `(${argsFmt}) -> (${retsFmt})`
      }
    }
  }

  module Local = {
    @genType
    type t = (array<string>, ValueType.t)

    @genType
    type index = int

    // run-length sequence of types
    @genType
    let fromTypes = (locals: array<(string, ValueType.t)>): array<t> => {
      let rec aux = (types, acc) => {
        switch (types, acc) {
        | (list{(x, t1), ...types}, list{}) => aux(types, list{(list{x}, t1)})
        | (list{(x, t1), ...types}, list{(lcls, t2), ...acc}) =>
          if t1 == t2 {
            aux(types, list{(list{x, ...lcls}, t2), ...acc})
          } else {
            aux(types, list{(list{x}, t1), (lcls, t2), ...acc})
          }
        | _ => acc
        }
      }

      let locals = aux(locals->List.fromArray, list{})->List.reverse
      locals->List.map(((names, ty)) => (names->List.toArray->Array.reverse, ty))->List.toArray
    }

    @genType
    let encode = ((names, typ): t): Vec.t => {
      Array.concat(uleb128(names->Array.length), [ValueType.encode(typ)])
    }

    @genType
    let show = ((names, ty): t) => {
      `local [${names->Array.joinWith(", ", x => x)}] ${ValueType.show(ty)}`
    }
  }

  module Body = {
    @genType
    type t = (array<Local.t>, array<Inst.t>)

    @genType
    let make = (locals, instructions): t => (locals, instructions)

    @genType
    let encode = ((locals, instructions): t): Vec.t => {
      let locals = Vec.encodeMany(locals->Array.map(Local.encode))
      let instructions = Array.concatMany(instructions->Array.map(Inst.encode))

      Array.concatMany([[locals->Array.length + instructions->Array.length], locals, instructions])
    }

    @genType
    let show = ((locals, insts): t, funcNames: array<string>, FuncSig(params, _): Signature.t) => {
      let indentation = ref(1)

      let paramNames = params->Array.mapWithIndex((i, _) => `param[${Int.toString(i)}]`)
      let localNames = Array.concat(
        paramNames,
        locals->Array.map(((names, _)) => names)->Array.concatMany,
      )

      let localsFmt = locals->Array.joinWith("\n", Local.show)

      let body =
        insts->Array.joinWith("\n", inst => Inst.show(inst, funcNames, localNames, ~indentation))

      if locals->Array.length == 0 {
        body
      } else {
        localsFmt ++ "\n" ++ body
      }
    }
  }

  @genType
  type t = (string, Signature.t, Body.t)

  @genType
  type index = int

  @genType
  let make = (name, signature, body): t => (name, signature, body)
}

module TypeSection = {
  // The Type Section consists of a list of function signatures.
  @genType
  type t = array<Func.Signature.t>

  @genType
  let make = (): t => []

  @genType
  let getOrAdd = (self: t, signature: Func.Signature.t): Func.Signature.index => {
    switch self->Array.getIndexBy(sig => sig == signature) {
    | Some(idx) => idx
    | None => {
        let _ = self->Js.Array2.push(signature)
        self->Array.length - 1
      }
    }
  }

  @genType
  let encode = (self: t): Vec.t => {
    Section.encode(Section.Type, Vec.encodeMany(self->Array.map(Func.Signature.encode)))
  }

  @genType
  let show = (self: t) => {
    Section.show(Section.Type) ++ "\n" ++ self->Array.joinWith("\n", Func.Signature.show)
  }
}

module FuncSection = {
  // The Function Section consists of an array of function declarations.
  // Its elements directly correspond to elements in the Code Section array.

  // A function declaration consists of:
  // an index in the Type Section of the signature of the function.
  @genType
  type t = array<Func.Signature.index>

  @genType
  let make = (): t => []

  @genType
  let addSignature = (self: t, signatureIndex: Func.Signature.index) => {
    let _ = self->Js.Array2.push(signatureIndex)
    self->Array.length - 1
  }

  @genType
  let encode = (self: t) => {
    Section.encode(Section.Function, Vec.encodeMany(self->Array.map(uleb128)))
  }

  @genType
  let show = (self: t, funcNames: array<string>) => {
    Section.show(Section.Function) ++
    "\n" ++
    self
    ->Array.zip(funcNames)
    ->Array.joinWith("\n", ((tyIdx, funcName)) => `${funcName}: ${tyIdx->Int.toString}`)
  }
}

module Str = {
  @genType
  let encode = (str: string): Vec.t => {
    Vec.encode(Js.String.split("", str)->Array.map(c => Int.fromFloat(Js.String.charCodeAt(0, c))))
  }
}

module ExportEntry = {
  module ExternalKind = {
    @genType
    type t = Func | Table | Memory | Global

    @genType
    let encode = (kind: t): Vec.t => [
      switch kind {
      | Func => 0
      | Table => 1
      | Memory => 2
      | Global => 3
      },
    ]

    @genType
    let show = kind =>
      switch kind {
      | Func => "func"
      | Table => "table"
      | Memory => "memory"
      | Global => "global"
      }
  }

  @genType
  type t = {
    name: string,
    kind: ExternalKind.t,
    index: int,
  }

  @genType
  let make = (name, kind, index): t => {name: name, kind: kind, index: index}

  @genType
  let encode = ({name, kind, index}: t): Vec.t => {
    Array.concatMany([name->Str.encode, kind->ExternalKind.encode, uleb128(index)])
  }

  @genType
  let show = ({name, kind, index}: t) => {
    `export ${ExternalKind.show(kind)} ${name} (index: ${Int.toString(index)})`
  }
}

module ExportSection = {
  @genType
  type t = array<ExportEntry.t>

  @genType
  let make = (): t => []

  @genType
  let add = (self: t, exp: ExportEntry.t): unit => {
    let _ = self->Js.Array2.push(exp)
  }

  @genType
  let encode = (self: t): Vec.t => {
    Section.encode(Section.Export, Vec.encodeMany(self->Array.map(ExportEntry.encode)))
  }

  @genType
  let show = (self: t) => {
    Section.show(Section.Export) ++ "\n" ++ self->Array.joinWith("\n", ExportEntry.show)
  }
}

module ReferenceType = {
  @genType
  type t = FuncRef | ExternRef

  @genType
  let show = ref =>
    switch ref {
    | FuncRef => "funcref"
    | ExternRef => "externref"
    }

  @genType
  let encode = ref =>
    switch ref {
    | FuncRef => 0x70
    | ExternRef => 0x6f
    }
}

module Limits = {
  @genType
  type t = (int, option<int>)

  @genType
  let show = ((min, max): t) => {
    `{ min: ${Int.toString(min)}, max: ${max->Option.mapWithDefault("?", Int.toString)} }`
  }

  @genType
  let make = (min: int, max: option<int>): t => (min, max)

  @genType
  let makeExact = (count: int): t => (count, Some(count))

  @genType
  let encode = ((min, max): t): Vec.t => {
    switch max {
    | Some(max) => Array.concatMany([[0x01], uleb128(min), uleb128(max)])
    | None => Array.concat([0x00], uleb128(min))
    }
  }
}

module Table = {
  @genType
  type index = int

  @genType
  type t = (ReferenceType.t, Limits.t)

  @genType
  let make = (refType: ReferenceType.t, limits: Limits.t): t => {
    (refType, limits)
  }

  @genType
  let encode = ((refTy, limits): t) => {
    Array.concat([refTy->ReferenceType.encode], limits->Limits.encode)
  }

  @genType
  let show = ((refTy, limits): t) => {
    `table ${refTy->ReferenceType.show} ${limits->Limits.show}`
  }
}

module TableSection = {
  @genType
  type t = array<Table.t>

  @genType
  let make = (): t => []

  @genType
  let add = (self: t, table: Table.t): unit => {
    let _ = self->Js.Array2.push(table)
  }

  @genType
  let show = tables => {
    Section.show(Section.Table) ++ "\n" ++ tables->Array.joinWith("\n", Table.show)
  }

  @genType
  let encode = tables => {
    Section.encode(Section.Table, Vec.encodeMany(tables->Array.map(Table.encode)))
  }
}

// https://webassembly.github.io/spec/core/binary/modules.html#element-section
// Element conficts the JS type
module Elem = {
  @genType
  type t = {
    offset: int,
    funcIndices: array<Func.index>,
  }

  @genType
  let fromFuncRefs = (~offset=0, funcIndices: array<Func.index>): t => {
    offset: offset,
    funcIndices: funcIndices,
  }

  @genType
  let show = ({offset, funcIndices}: t) =>
    `elem funcref (${funcIndices->Array.joinWith(" ", Int.toString)}) (offset=${Int.toString(
        offset,
      )})`

  @genType
  let encode = ({offset, funcIndices}: t): Vec.t => {
    Array.concatMany([
      [0x00],
      Inst.ConstI32(offset)->Inst.encode,
      Inst.End->Inst.encode,
      Vec.encodeMany(funcIndices->Array.map(uleb128)),
    ])
  }
}

module ElementSection = {
  @genType
  type t = array<Elem.t>

  @genType
  let make = (): t => []

  @genType
  let add = (self: t, elem: Elem.t): unit => {
    let _ = self->Js.Array2.push(elem)
  }

  @genType
  let show = elems => {
    Section.show(Section.Element) ++ "\n" ++ elems->Array.joinWith("\n", Elem.show)
  }

  @genType
  let encode = elems => {
    Section.encode(Section.Element, Vec.encodeMany(elems->Array.map(Elem.encode)))
  }
}

module Global = {
  module Initializer = {
    @genType
    type t = array<Inst.t>

    let fromInstructions = (instructions): t => instructions

    @genType
    let encode = (insts: t): Vec.t => {
      Array.concat(Array.concatMany(insts->Array.map(Inst.encode)), Inst.encode(Inst.End))
    }

    @genType
    let show = insts => {
      switch insts {
      | [inst] => inst->Inst.show([], [])
      | _ => "\n" ++ insts->Array.joinWith("\n", inst => "  " ++ inst->Inst.show([], [])) ++ "\nend"
      }
    }
  }

  @genType
  type t = (ValueType.t, bool, Initializer.t)

  @genType
  type index = int

  @genType
  let make = (~isMutable, ty, init): t => (ty, isMutable, init)

  @genType
  let show = ((ty, mut, init): t) => {
    `global ${mut ? "mutable" : "immutable"} ` ++
    ty->ValueType.show ++
    " = " ++
    init->Initializer.show
  }

  @genType
  let encode = ((ty, mut, init): t): Vec.t => {
    Array.concat([ty->ValueType.encode, mut ? 0x1 : 0x0], init->Initializer.encode)
  }
}

module GlobalSection = {
  @genType
  type t = array<Global.t>

  @genType
  let make = (): t => []

  @genType
  let add = (self: t, global: Global.t): unit => {
    let _ = self->Js.Array2.push(global)
  }

  @genType
  let show = (globals: t) => {
    Section.show(Section.Global) ++ "\n" ++ globals->Array.joinWith("\n", Global.show)
  }

  @genType
  let encode = (globals: t): Vec.t => {
    Section.encode(Section.Global, Vec.encodeMany(globals->Array.map(Global.encode)))
  }
}

module Memory = {
  @genType
  type t = Limits.t
  @genType
  type index = int

  @genType
  let make = (limits: Limits.t): t => limits

  @genType
  let show = (mem: t) => "memory " ++ mem->Limits.show

  @genType
  let encode = Limits.encode
}

module MemorySection = {
  @genType
  type t = array<Memory.t>

  @genType
  let make = (): t => []

  @genType
  let add = (self: t, mem: Memory.t): Memory.index => {
    let index = self->Js.Array2.push(mem) - 1
    index
  }

  @genType
  let show = (mems: t) => {
    Section.show(Section.Memory) ++ "\n" ++ mems->Array.joinWith("\n", Memory.show)
  }

  @genType
  let encode = (mems: t): Vec.t => {
    Section.encode(Section.Memory, Vec.encodeMany(mems->Array.map(Memory.encode)))
  }
}

module Import = {
  @genType
  type index = int
}

module ImportEntry = {
  module ImportDesc = {
    @genType
    type t = Func(Func.Signature.index)

    @genType
    let make = ()

    @genType
    let encode = (desc: t): Vec.t =>
      switch desc {
      | Func(funcSigIdx) => Array.concatMany([[0x00], uleb128(funcSigIdx)])
      }

    @genType
    let show = (desc: t): string => {
      switch desc {
      | Func(funcSigIdx) => `func (index: ${Int.toString(funcSigIdx)})`
      }
    }
  }

  @genType
  type t = {moduleName: string, entityName: string, desc: ImportDesc.t}

  @genType
  let encode = ({moduleName, entityName, desc}: t): Vec.t => {
    Array.concatMany([moduleName->Str.encode, entityName->Str.encode, desc->ImportDesc.encode])
  }

  @genType
  let make = (moduleName, entityName, desc): t => {
    moduleName: moduleName,
    entityName: entityName,
    desc: desc,
  }

  @genType
  let show = ({moduleName, entityName, desc}: t) => {
    `import ${ImportDesc.show(desc)} ${moduleName}.${entityName}`
  }
}

module ImportSection = {
  @genType
  type t = array<ImportEntry.t>

  @genType
  let make = (): t => []

  @genType
  let add = (self: t, imp: ImportEntry.t): int => {
    let index = self->Js.Array2.push(imp) - 1
    index
  }

  @genType
  let encode = (self: t): Vec.t => {
    Section.encode(Section.Import, Vec.encodeMany(self->Array.map(ImportEntry.encode)))
  }

  @genType
  let show = (self: t) => {
    Section.show(Section.Import) ++ "\n" ++ self->Array.joinWith("\n", ImportEntry.show)
  }

  @genType
  let count = (self: t) => self->Array.length
}

module CodeSection = {
  @genType
  type t = array<Func.Body.t>

  @genType
  let make = (): t => []

  @genType
  let addFunc = (self: t, f): Func.index => {
    let _ = self->Js.Array2.push(f)
    self->Array.length - 1
  }

  @genType
  let encode = (bodies: t): Vec.t => {
    Section.encode(Section.Code, Vec.encodeMany(bodies->Array.map(Func.Body.encode)))
  }

  @genType
  let show = (self: t, imports: array<ImportEntry.t>, funcs: array<Func.t>) => {
    let importFuncNames =
      imports
      ->Array.keep(({desc}) =>
        switch desc {
        | Func(_) => true
        }
      )
      ->Array.map(({entityName}) => entityName)

    let funcNames = Array.concat(importFuncNames, funcs->Array.map(((name, _, _)) => name))

    Section.show(Section.Code) ++
    "\n" ++
    self
    ->Array.zip(funcs)
    ->Array.joinWith("\n\n", ((b, (name, sig, _))) => {
      `${name}\n` ++ b->Func.Body.show(funcNames, sig)
    })
  }
}

module Data = {
  type index = int
  type memoryIndex = int
  type offset = int

  module Mode = {
    type t = Passive | Active(memoryIndex, offset)

    let show = (mode: t) => {
      switch mode {
      | Passive => "passive"
      | Active(m, offset) => `active memIndex: ${Int.toString(m)} offset: ${Int.toString(offset)}`
      }
    }
  }

  module Segment = {
    type t = {init: array<byte>, mode: Mode.t}

    let make = (init: array<byte>, mode: Mode.t): t => {
      init: init,
      mode: mode,
    }

    let show = ({init, mode}: t) => {
      `segment len: ${Int.toString(init->Array.length)} mode: ${mode->Mode.show}`
    }

    let encode = ({init, mode}: t): Vec.t => {
      switch mode {
      | Active(0, offset) =>
        Array.concatMany([
          [0x00],
          Inst.ConstI32(offset)->Inst.encode,
          Inst.End->Inst.encode,
          Vec.encode(init),
        ])
      | Passive => Array.concat([0x01], Vec.encode(init))
      | Active(m, offset) =>
        Array.concatMany([
          [0x02],
          uleb128(m),
          Inst.ConstI32(offset)->Inst.encode,
          Inst.End->Inst.encode,
          Vec.encode(init),
        ])
      }
    }
  }

  module Section = {
    type t = array<Segment.t>

    let make = (): t => []

    let add = (self: t, seg: Segment.t): index => {
      let index = self->Js.Array2.push(seg) - 1
      index
    }

    let show = (segs: t) => {
      Section.show(Section.Data) ++ "\n" ++ segs->Array.joinWith("\n\n", Segment.show)
    }

    @genType
    let encode = (segs: t): Vec.t => {
      Section.encode(Section.Data, Vec.encodeMany(segs->Array.map(Segment.encode)))
    }
  }
}

module Module = {
  @genType
  type t = {
    typeSection: TypeSection.t,
    importSection: ImportSection.t,
    funcSection: FuncSection.t,
    tableSection: TableSection.t,
    memorySection: MemorySection.t,
    globalSection: GlobalSection.t,
    exportSection: ExportSection.t,
    elementSection: ElementSection.t,
    codeSection: CodeSection.t,
    dataSection: Data.Section.t,
    funcs: array<Func.t>,
  }

  @genType
  let make = (): t => {
    typeSection: TypeSection.make(),
    importSection: ImportSection.make(),
    funcSection: FuncSection.make(),
    tableSection: TableSection.make(),
    memorySection: MemorySection.make(),
    globalSection: GlobalSection.make(),
    exportSection: ExportSection.make(),
    elementSection: ElementSection.make(),
    codeSection: CodeSection.make(),
    dataSection: Data.Section.make(),
    funcs: [],
  }

  @genType
  let getOrAddSignature = (self: t, sig: Func.Signature.t): Func.Signature.index => {
    self.typeSection->TypeSection.getOrAdd(sig)
  }

  @genType
  let addFunc = (self: t, name, sig: Func.Signature.t, body: Func.Body.t): Func.index => {
    let sigIndex = self->getOrAddSignature(sig)
    let _ = self.funcSection->FuncSection.addSignature(sigIndex)
    let _ = self.funcs->Js.Array2.push(Func.make(name, sig, body))
    self.codeSection->CodeSection.addFunc(body) + self.importSection->ImportSection.count
  }

  @genType
  let addExportedFunc = (
    self: t,
    name: string,
    sig: Func.Signature.t,
    body: Func.Body.t,
  ): Func.index => {
    let funcIndex = self->addFunc(name, sig, body)
    self.exportSection->ExportSection.add(
      ExportEntry.make(name, ExportEntry.ExternalKind.Func, funcIndex),
    )

    funcIndex
  }

  @genType
  let addTable = (self: t, table: Table.t): unit => {
    self.tableSection->TableSection.add(table)
  }

  @genType
  let addElement = (self: t, elem: Elem.t): unit => {
    self.elementSection->ElementSection.add(elem)
  }

  @genType
  let addGlobal = (self: t, global: Global.t): unit => {
    self.globalSection->GlobalSection.add(global)
  }

  @genType
  let addMemory = (self: t, mem: Memory.t): Memory.index => {
    self.memorySection->MemorySection.add(mem)
  }

  @genType
  let addImport = (self: t, imp: ImportEntry.t): Import.index => {
    self.importSection->ImportSection.add(imp)
  }

  @genType
  let addData = (self: t, segment: Data.Segment.t): Data.index => {
    self.dataSection->Data.Section.add(segment)
  }

  @genType
  let encode = (self: t): Vec.t => {
    Array.concatMany([
      [0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00], // magic cookie "\0asm" and wasm version
      self.typeSection->TypeSection.encode,
      self.importSection->ImportSection.encode,
      self.funcSection->FuncSection.encode,
      self.tableSection->TableSection.encode,
      self.memorySection->MemorySection.encode,
      self.globalSection->GlobalSection.encode,
      self.exportSection->ExportSection.encode,
      self.elementSection->ElementSection.encode,
      self.codeSection->CodeSection.encode,
      self.dataSection->Data.Section.encode,
    ])
  }

  @genType
  let encodeAsUint8Array = (self: t): Js.Typed_array.Uint8Array.t => {
    %raw(`(bytes => Uint8Array.from(bytes))`)(self->encode)
  }

  @genType
  let show = (self: t) => {
    [
      self.typeSection->TypeSection.show,
      self.importSection->ImportSection.show,
      self.funcSection->FuncSection.show(self.funcs->Array.map(((name, _, _)) => name)),
      self.tableSection->TableSection.show,
      self.memorySection->MemorySection.show,
      self.globalSection->GlobalSection.show,
      self.exportSection->ExportSection.show,
      self.elementSection->ElementSection.show,
      self.codeSection->CodeSection.show(self.importSection, self.funcs),
      self.dataSection->Data.Section.show,
    ]->Array.joinWith("\n\n", x => x)
  }
}
