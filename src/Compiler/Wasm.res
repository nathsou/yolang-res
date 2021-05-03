open Belt

type byte = int

// https://www.wikiwand.com/en/LEB128
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
  type t = array<byte>
  let encode = (vec: t) => Array.concat(uleb128(vec->Array.length), vec)
  let encodeMany = (vecs: array<t>): t => Array.concat(uleb128(vecs->Array.length), Array.concatMany(vecs))
}

module F32 = {
  type t = float

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
  type t = float

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

  let encode = (section, bytes): Vec.t => Array.concat([section->id], Vec.encode(bytes))

  let show = sec => "[" ++ switch sec {
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
  } ++ " section]"
}

module ValueType = {
  type t = I32 | I64 | F32 | F64

  let encode = (v: t): byte =>
    switch v {
    | I32 => 0x7f
    | I64 => 0x7e
    | F32 => 0x7d
    | F64 => 0x7c
    }

    let show = v => switch v {
      | I32 => "i32"
      | I64 => "i64"
      | F32 => "f32"
      | F64 => "f64"
    } 
}

module BlockReturnType = {
  type t = I32 | I64 | F32 | F64 | Void

  let encode = (v: t): byte =>
    switch v {
    | I32 => 0x7f
    | I64 => 0x7e
    | F32 => 0x7d
    | F64 => 0x7c
    | Void => 0x40
    }

    let show = v => switch v {
      | I32 => "i32"
      | I64 => "i64"
      | F32 => "f32"
      | F64 => "f64"
      | Void => "void"
    } 
}

module Inst = {
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
    | GetLocal(int)
    | SetLocal(int)
    | Call(int)
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

  let info = inst =>
    switch inst {
    | ConstI32(n) => (0x41, `i32.const ${Int.toString(n)}`)
    | ConstI64(n) => (0x42, `i64.const ${Int.toString(n)}`)
    | ConstF32(x) => (0x43, `i64.const ${Float.toString(x)}`)
    | ConstF64(x) => (0x44, `i64.const ${Float.toString(x)}`)
    | If(_) => (0x04, "if")
    | BranchIf(lvl) => (0x0d, `br_if ${Int.toString(lvl)}`)
    | Branch(lvl) => (0x0c, `br ${Int.toString(lvl)}`)
    | Block(_) => (0x02, "block")
    | Loop(_) => (0x03, "loop")
    | Else => (0x05, "else")
    | End => (0x0b, "end")
    | Return => (0x0f, "return")
    | GetLocal(idx) => (0x20, "local.get " ++ Int.toString(idx))
    | SetLocal(idx) => (0x21, "local.get " ++ Int.toString(idx))
    | Call(funcIdx) => (0x10, "call " ++ Int.toString(funcIdx))
    | Drop => (0x1a, "drop")
    | AddI32 => (0x6a, "i32.add")
    | SubI32 => (0x6b, "i32.sub")
    | MulI32 => (0x6c, "i32.mul")
    | DivI32Signed => (0x6d, "i32.div_u")
    | DivI32Unsigned => (0x6e, "i32.div_s")
    | RemI32Signed => (0x6f, "i32.rem_s")
    | RemI32Unsigned => (0x70, "i32.rem_u")
    | EqzI32 => (0x45, "i32.eqz")
    | EqI32 => (0x46, "i32.eq")
    | EqI64 => (0x51, "i64.eq")
    | EqF32 => (0x5b, "f32.eq")
    | EqF64 => (0x61, "f64.eq")
    | NeI32 => (0x47, "i32.ne")
    | NeI64 => (0x52, "i64.ne")
    | NeF32 => (0x5c, "f32.ne")
    | NeF64 => (0x62, "f64.ne")
    | LeI32Signed => (0x4c, "i32.le_s")
    | LeI32Unsigned => (0x4d, "i32.le_u")
    | LeI64Signed => (0x57, "i64.le_s")
    | LeI64Unsigned => (0x58, "i64.le_u")
    | LeF32 => (0x5f, "f32.le")
    | LeF64 => (0x65, "f64.le")
    | GeI32Signed => (0x4e, "i32.ge_s")
    | GeI32Unsigned => (0x4f, "i32.ge_u")
    | GeI64Signed => (0x59, "i64.ge_s")
    | GeI64Unsigned => (0x5a, "i64.ge_u")
    | GeF32 => (0x60, "f32.ge")
    | GeF64 => (0x66, "f64.ge")
    | LtI32Signed => (0x48, "i32.lt_s")
    | LtI32Unsigned => (0x49, "i32.lt_u")
    | LtI64Signed => (0x53, "i64.lt_s")
    | LtI64Unsigned => (0x54, "i64.lt_u")
    | LtF32 => (0x5d, "f32.lt")
    | LtF64 => (0x63, "f64.lt")
    | GtI32Signed => (0x4a, "i32.gt_s")
    | GtI32Unsigned => (0x4b, "i32.gt_u")
    | GtI64Signed => (0x55, "i64.gt_s")
    | GtI64Unsigned => (0x56, "i64.gt_u")
    | GtF32 => (0x5e, "f32.gt")
    | GtF64 => (0x64, "f64.gt")
    }

  let opcode = inst => {
    let (opcode, _) = inst->info
    opcode
  }

  let show = inst => {
    let (_, fmt) = inst->info
    fmt
  }

  let encode = inst =>
    switch inst {
    | ConstI32(n) => Array.concat([inst->opcode], sleb128(n))
    | ConstI64(n) => Array.concat([inst->opcode], sleb128(n))
    | ConstF32(x) => Array.concat([inst->opcode], F32.encode(x))
    | ConstF64(x) => Array.concat([inst->opcode], F64.encode(x))
    | GetLocal(n) => Array.concat([inst->opcode], uleb128(n))
    | SetLocal(n) => Array.concat([inst->opcode], uleb128(n))
    | If(bt) => [inst->opcode, bt->BlockReturnType.encode]
    | Block(bt) => [inst->opcode, bt->BlockReturnType.encode]
    | Loop(bt) => [inst->opcode, bt->BlockReturnType.encode]
    | BranchIf(depth) => Array.concat([inst->opcode], uleb128(depth))
    | Branch(depth) => Array.concat([inst->opcode], uleb128(depth))
    | Call(funcIdx) => Array.concat([inst->opcode], uleb128(funcIdx)) 
    | _ => [inst->opcode]
    }

}

module Func = {
  module Signature = {
    type t = FuncSig(array<ValueType.t>, array<ValueType.t>)
    type index = int

    // http://webassembly.github.io/spec/core/binary/types.html#function-types
    let funcTypeCode = 0x60

    let make = (params, ret) => {
      FuncSig(params, ret->Option.mapWithDefault([], r => [r]))
    }

    let empty = make([], None)

    let encode = (FuncSig(params, ret)): Vec.t => {
      Array.concatMany([
        [funcTypeCode],
        Vec.encode(params->Array.map(ValueType.encode)),
        Vec.encode(ret->Array.map(ValueType.encode)),
      ])
    }

    let show = (FuncSig(args, rets)) => {
     let args = args->Array.joinWith(", ", ValueType.show)
     let rets = rets->Array.joinWith(", ", ValueType.show)

     `(${args}) -> ${rets}`
    }
  }

  module Locals = {
    type t = (int, ValueType.t)

    type index = int

    // run-length sequence of types
    let fromTypes = (types: array<ValueType.t>): array<t> => {
      let rec aux = (types, acc) => {
        switch (types, acc) {
        | (list{t1, ...types}, list{}) => aux(types, list{(1, t1)})
        | (list{t1, ...types}, list{(count, t2), ...acc}) =>
          if t1 == t2 {
            aux(types, list{(count + 1, t2), ...acc})
          } else {
            aux(types, list{(1, t1), (count, t2), ...acc})
          }
        | _ => acc
        }
      }

      aux(types->List.fromArray, list{})->List.reverse->List.toArray
    }

    let encode = ((count, typ)): Vec.t => {
      Array.concat(uleb128(count), [ValueType.encode(typ)])
    }

    let show = ((count, ty): t) => {
      `local ${Int.toString(count)} ${ValueType.show(ty)}`
    } 
  }

  module Body = {
    type t = (array<Locals.t>, array<Inst.t>)

    let make = (locals, instructions): t => (locals, instructions)

    let encode = ((locals, instructions): t): Vec.t => {
      let locals = Vec.encodeMany(locals->Array.map(Locals.encode))
      let instructions = Array.concatMany(
        instructions->Array.map(Inst.encode),
      )

      Array.concatMany([[locals->Array.length + instructions->Array.length], locals, instructions])
    }

    let show = ((locals, insts): t) => {
      let locals = locals->Array.joinWith("\n", Locals.show)
      let body = insts->Array.joinWith("\n", Inst.show)

      locals ++ "\n" ++ body
    } 
  }

  type t = (Signature.t, Body.t)
  type index = int

  let make = (signature, body): t => (signature, body)
}

module TypeSection = {
  // The Type Section consists of an list of function signatures.
  type t = array<Func.Signature.t>

  let make = (): t => []

  let add = (self: t, signature: Func.Signature.t): Func.Signature.index => {
    switch self->Array.getIndexBy(sig => sig == signature) {
    | Some(idx) => idx
    | None => {
        let _ = self->Js.Array2.push(signature)
        self->Array.length - 1
      }
    }
  }

  let encode = (self: t): Vec.t => {
    Section.encode(Section.Type, Vec.encodeMany(self->Array.map(Func.Signature.encode)))
  }

  let show = (self: t) => {
    Section.show(Section.Type) ++ ":\n" ++ self->Array.joinWith("\n", Func.Signature.show)
  }
}

module FuncSection = {
  // The Function Section consists of an array of function declarations.
  // Its elements directly correspond to elements in the Code Section array.

  // A function declaration consists of:
  // an index in the Type Section of the signature of the function.
  type t = array<Func.Signature.index>

  let make = (): t => []

  let addSignature = (self: t, signatureIndex: Func.Signature.index) => {
    let _ = self->Js.Array2.push(signatureIndex)
    self->Array.length - 1
  }

  let encode = (self: t) => {
    Section.encode(Section.Function, Vec.encodeMany(self->Array.map(uleb128)))
  }

  let show = (self: t) => {
    Section.show(Section.Function) ++ ": " ++ self->Array.joinWith(", ", Int.toString)
  }
}

module CodeSection = {
  type t = array<Func.Body.t>

  let make = (): t => []

  let addFunc = (self: t, f): Func.index => {
    let _ = self->Js.Array2.push(f)
    self->Array.length - 1
  }

  let encode = (bodies: t): Vec.t => {
    Section.encode(Section.Code, Vec.encodeMany(bodies->Array.map(Func.Body.encode)))
  }

  let show = (self: t) => {
    Section.show(Section.Code) ++ ":\n" ++ self->Array.joinWith("\n\n", Func.Body.show)
  }
}

module String = {
  let encode = (str: string): Vec.t => {
    Vec.encode(Js.String.split("", str)->Array.map(c => Int.fromFloat(Js.String.charCodeAt(0, c))))
  }
}

module ExportEntry = {
  module ExternalKind = {
    type t = Func | Table | Memory | Global

    let encode = (kind: t): Vec.t => [switch kind {
      | Func => 0
      | Table => 1
      | Memory => 2
      | Global => 3
    }]

    let show = kind => switch kind {
      | Func => "func"
      | Table => "table"
      | Memory => "memory"
      | Global => "global"
    }
  }

  type t = {
    name: string,
    kind: ExternalKind.t,
    index: int
  }

  let make = (name, kind, index): t => { name, kind, index }

  let encode = ({name, kind, index}: t): Vec.t => {
    Array.concatMany([name->String.encode, kind->ExternalKind.encode, uleb128(index)])
  }

  let show = ({name, kind, index}: t) => {
    `export ${ExternalKind.show(kind)} ${name} (index: ${Int.toString(index)})`
  }
}

module ExportSection = {
  type t = array<ExportEntry.t>

  let make = (): t => []

  let addExport = (self: t, exp: ExportEntry.t): unit => {
    let _ = self->Js.Array2.push(exp)
  }

  let encode = (self: t): Vec.t => {
    Section.encode(Section.Export, Vec.encodeMany(self->Array.map(ExportEntry.encode)))
  }

  let show = (self: t) => {
    Section.show(Section.Export) ++ ":\n" ++ self->Array.joinWith("\n", ExportEntry.show)
  }
}

module Module = {
  type t = {
    typeSection: TypeSection.t,
    funcSection: FuncSection.t,
    exportSection: ExportSection.t,
    codeSection: CodeSection.t,
    importsCount: int,
  }

  let make = (): t => {
    typeSection: TypeSection.make(),
    funcSection: FuncSection.make(),
    exportSection: ExportSection.make(),
    codeSection: CodeSection.make(),
    importsCount: 0,
  }

  let addFunc = (self: t, sig: Func.Signature.t, body: Func.Body.t): Func.index => {
    let sigIndex = self.typeSection->TypeSection.add(sig)
    let _ = self.funcSection->FuncSection.addSignature(sigIndex)
    self.codeSection->CodeSection.addFunc(body) + self.importsCount
  }

  let addExportedFunc = (self: t, name: string, sig: Func.Signature.t, body: Func.Body.t): Func.index => {
    let funcIndex = self->addFunc(sig, body)
    self.exportSection->ExportSection.addExport(ExportEntry.make(name, ExportEntry.ExternalKind.Func, funcIndex))

    funcIndex
  }

  let encode = (self: t): Vec.t => {
    Array.concatMany([
      [0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00], // magic cookie "\0asm" and wasm version
      self.typeSection->TypeSection.encode,
      self.funcSection->FuncSection.encode,
      self.exportSection->ExportSection.encode,
      self.codeSection->CodeSection.encode
    ])
  }

  let show = (self: t) => {
    [
      self.typeSection->TypeSection.show,
      self.funcSection->FuncSection.show,
      self.exportSection->ExportSection.show,
      self.codeSection->CodeSection.show
    ]->Array.joinWith("\n\n", x => x)
  }
}
