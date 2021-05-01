open Belt

type byte = int

module Vec = {
  type t = array<byte>
  let encode = (vec: t) => Array.concat([vec->Array.length], vec)
  let encodeMany = (vecs: array<t>): t => Array.concat([vecs->Array.length], Array.concatMany(vecs))
}

// https://www.wikiwand.com/en/LEB128
let uleb128 = (n: int): Vec.t => {
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

let sleb128 = (val: int): Vec.t => {
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
}

module Inst = {
  type t =
    | ConstI32(int)
    | ConstI64(int)
    | ConstF32(float)
    | ConstF64(float)
    | End
    | GetLocal(int)
    | SetLocal(int)
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

  let opcode = inst =>
    switch inst {
    | ConstI32(_) => 0x41
    | ConstI64(_) => 0x42
    | ConstF32(_) => 0x43
    | ConstF64(_) => 0x44
    | End => 0x0b
    | GetLocal(_) => 0x20
    | SetLocal(_) => 0x21
    | AddI32 => 0x6a
    | SubI32 => 0x6b
    | MulI32 => 0x6c
    | DivI32Signed => 0x6d
    | DivI32Unsigned => 0x6e
    | EqzI32 => 0x45
    | EqI32 => 0x46
    | EqI64 => 0x51
    | EqF32 => 0x5b
    | EqF64 => 0x61
    | NeI32 => 0x47
    | NeI64 => 0x52
    | NeF32 => 0x5c
    | NeF64 => 0x62
    | LeI32Signed => 0x4c
    | LeI32Unsigned => 0x4d
    | LeI64Signed => 0x57
    | LeI64Unsigned => 0x58
    | LeF32 => 0x5f
    | LeF64 => 0x65
    | GeI32Signed => 0x4e
    | GeI32Unsigned => 0x4f
    | GeI64Signed => 0x59
    | GeI64Unsigned => 0x5a
    | GeF32 => 0x60
    | GeF64 => 0x66
    | LtI32Signed => 0x48
    | LtI32Unsigned => 0x49
    | LtI64Signed => 0x53
    | LtI64Unsigned => 0x54
    | LtF32 => 0x5d
    | LtF64 => 0x63
    | GtI32Signed => 0x4a
    | GtI32Unsigned => 0x4b
    | GtI64Signed => 0x55
    | GtI64Unsigned => 0x56
    | GtF32 => 0x5e
    | GtF64 => 0x64
    }

  let encode = inst =>
    switch inst {
    | ConstI32(n) => Array.concat([inst->opcode], sleb128(n))
    | ConstI64(n) => Array.concat([inst->opcode], sleb128(n))
    | ConstF32(x) => Array.concat([inst->opcode], F32.encode(x))
    | ConstF64(x) => Array.concat([inst->opcode], F64.encode(x))
    | GetLocal(n) => Array.concat([inst->opcode], uleb128(n))
    | SetLocal(n) => Array.concat([inst->opcode], uleb128(n))
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
  }

  module Locals = {
    type t = (int, ValueType.t)

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
  }

  module Body = {
    type t = (array<Locals.t>, array<Inst.t>)

    let make = (locals, instructions): t => (locals, instructions)

    let encode = ((locals, instructions): t): Vec.t => {
      let locals = Vec.encodeMany(locals->Array.map(Locals.encode))
      let instructions = Array.concatMany(
        Array.concat(instructions, [Inst.End])->Array.map(Inst.encode),
      )

      Array.concatMany([[locals->Array.length + instructions->Array.length], locals, instructions])
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

  let addMut = (self: t, signature: Func.Signature.t): Func.Signature.index => {
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
}

module FuncSection = {
  // The Function Section consists of an array of function declarations.
  // Its elements directly correspond to elements in the Code Section array.

  // A function declaration consists of:
  // an index in the Type Section of the signature of the function.
  type t = array<Func.Signature.index>

  let make = (): t => []

  let addSignatureMut = (self: t, signatureIndex: Func.Signature.index) => {
    let _ = self->Js.Array2.push(signatureIndex)
    self->Array.length - 1
  }

  let encode = (self: t) =>
    Section.encode(Section.Function, Vec.encodeMany(self->Array.map(uleb128)))
}

module CodeSection = {
  type t = array<Func.Body.t>

  let make = (): t => []

  let addFuncMut = (self: t, f): Func.index => {
    let _ = self->Js.Array2.push(f)
    self->Array.length - 1
  }

  let encode = (bodies: t): Vec.t => {
    Section.encode(Section.Code, Vec.encodeMany(bodies->Array.map(Func.Body.encode)))
  }
}

module Module = {
  type t = {
    typeSection: TypeSection.t,
    funcSection: FuncSection.t,
    codeSection: CodeSection.t,
    importsCount: int,
  }

  let make = (): t => {
    typeSection: TypeSection.make(),
    funcSection: FuncSection.make(),
    codeSection: CodeSection.make(),
    importsCount: 0,
  }

  let addFuncMut = (self: t, (sig, body): Func.t): Func.index => {
    let sigIndex = self.typeSection->TypeSection.addMut(sig)
    let _ = self.funcSection->FuncSection.addSignatureMut(sigIndex)
    self.codeSection->CodeSection.addFuncMut(body) + self.importsCount
  }

  let encode = (self: t): Vec.t => {
    Array.concatMany([
      [0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00], // magic cookie "\0asm" and wasm version
      self.typeSection->TypeSection.encode,
      self.funcSection->FuncSection.encode,
      self.codeSection->CodeSection.encode,
    ])
  }
}
