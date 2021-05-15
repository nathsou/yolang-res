open Belt

let peephole = (insts: array<Wasm.Inst.t>) => {
  open Wasm.Inst

  let rec aux = (insts: list<t>, acc: list<t>): array<t> => {
    switch insts {
    | list{ConstI32(_), Drop, ...tl} => aux(tl, acc)
    | list{ConstI64(_), Drop, ...tl} => aux(tl, acc)
    | list{ConstF32(_), Drop, ...tl} => aux(tl, acc)
    | list{ConstF64(_), Drop, ...tl} => aux(tl, acc)
    | list{GetLocal(_), Drop, ...tl} => aux(tl, acc)
    | list{Return, Drop, ...tl} => aux(list{Return, ...tl}, acc)
    | list{SetLocal(a), GetLocal(b), ...tl} if a == b => aux(list{TeeLocal(a), ...tl}, acc)
    | list{LeI32Unsigned, EqzI32, ...tl} => aux(list{GtI32Unsigned, ...tl}, acc)
    | list{GtI32Unsigned, EqzI32, ...tl} => aux(list{LeI32Unsigned, ...tl}, acc)
    | list{GeI32Unsigned, EqzI32, ...tl} => aux(list{LtI32Unsigned, ...tl}, acc)
    | list{LtI32Unsigned, EqzI32, ...tl} => aux(list{GeI32Unsigned, ...tl}, acc)
    | list{LeI32Signed, EqzI32, ...tl} => aux(list{GtI32Signed, ...tl}, acc)
    | list{GtI32Signed, EqzI32, ...tl} => aux(list{LeI32Signed, ...tl}, acc)
    | list{GeI32Signed, EqzI32, ...tl} => aux(list{LtI32Signed, ...tl}, acc)
    | list{LtI32Signed, EqzI32, ...tl} => aux(list{GeI32Signed, ...tl}, acc)
    | list{EqI32, EqzI32, ...tl} => aux(list{NeI32, ...tl}, acc)
    | list{NeI32, EqzI32, ...tl} => aux(list{EqI32, ...tl}, acc)
    | list{EqzI32, EqzI32, ...tl} => aux(tl, acc)
    | list{ConstI32(a), ConstI32(b), AddI32, ...tl} => aux(list{ConstI32(a + b), ...tl}, acc)
    | list{ConstI32(a), ConstI32(b), MulI32, ...tl} => aux(list{ConstI32(a * b), ...tl}, acc)
    | list{ConstI32(a), ConstI32(b), SubI32, ...tl} => aux(list{ConstI32(a - b), ...tl}, acc)
    | list{ConstI32(a), ConstI32(b), DivI32Unsigned, ...tl} =>
      aux(list{ConstI32(a / b), ...tl}, acc)
    | list{ConstI32(a), ConstI32(b), RemI32Unsigned, ...tl} =>
      aux(list{ConstI32(mod(a, b)), ...tl}, acc)
    | list{ConstI32(0), EqI32, ...tl} => aux(list{EqzI32, ...tl}, acc)
    | list{ConstI32(a), ConstI32(b), EqI32, ...tl} =>
      aux(list{ConstI32(a == b ? 1 : 0), ...tl}, acc)
    | list{ConstI32(a), ConstI32(b), NeI32, ...tl} =>
      aux(list{ConstI32(a == b ? 0 : 1), ...tl}, acc)
    | list{ConstI32(funcIdx), CallIndirect(_, _), ...tl} => aux(list{Call(funcIdx), ...tl}, acc)
    | list{h, ...tl} => aux(tl, list{h, ...acc})
    | list{} => acc->List.toArray->Array.reverse
    }
  }

  aux(insts->List.fromArray, list{})
}
