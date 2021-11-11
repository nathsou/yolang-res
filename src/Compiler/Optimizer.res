open Belt

let peephole = (~maxPasses=100, insts: array<Wasm.Inst.t>) => {
  open Wasm.Inst

  let instsCount = ref(insts->Array.length)

  let rec aux = (insts: list<t>, acc: list<t>, maxPassesLeft): list<t> => {
    let go = insts => aux(insts, acc, maxPassesLeft)
    switch insts {
    | list{SetLocal(a), GetLocal(b), ...tl} if a == b => go(list{TeeLocal(a), ...tl})
    | list{ConstI32(_), Drop, ...tl} => go(tl)
    | list{ConstI64(_), Drop, ...tl} => go(tl)
    | list{ConstF32(_), Drop, ...tl} => go(tl)
    | list{ConstF64(_), Drop, ...tl} => go(tl)
    | list{GetLocal(_), Drop, ...tl} => go(tl)
    | list{GetGlobal(_), Drop, ...tl} => go(tl)
    | list{TeeLocal(a), Drop, ...tl} => go(list{SetLocal(a), ...tl})
    | list{Return, Drop, ...tl} => go(list{Return, ...tl})
    | list{LeI32Unsigned, EqzI32, ...tl} => go(list{GtI32Unsigned, ...tl})
    | list{GtI32Unsigned, EqzI32, ...tl} => go(list{LeI32Unsigned, ...tl})
    | list{GeI32Unsigned, EqzI32, ...tl} => go(list{LtI32Unsigned, ...tl})
    | list{LtI32Unsigned, EqzI32, ...tl} => go(list{GeI32Unsigned, ...tl})
    | list{LeI32Signed, EqzI32, ...tl} => go(list{GtI32Signed, ...tl})
    | list{GtI32Signed, EqzI32, ...tl} => go(list{LeI32Signed, ...tl})
    | list{GeI32Signed, EqzI32, ...tl} => go(list{LtI32Signed, ...tl})
    | list{LtI32Signed, EqzI32, ...tl} => go(list{GeI32Signed, ...tl})
    | list{EqI32, EqzI32, ...tl} => go(list{NeI32, ...tl})
    | list{NeI32, EqzI32, ...tl} => go(list{EqI32, ...tl})
    | list{EqzI32, EqzI32, ...tl} => go(tl)
    | list{ConstI32(a), ConstI32(b), AddI32, ...tl} => go(list{ConstI32(a + b), ...tl})
    | list{ConstI32(a), ConstI32(b), MulI32, ...tl} => go(list{ConstI32(a * b), ...tl})
    | list{ConstI32(a), ConstI32(b), SubI32, ...tl} => go(list{ConstI32(a - b), ...tl})
    | list{ConstI32(a), ConstI32(b), DivI32Unsigned, ...tl} => go(list{ConstI32(a / b), ...tl})
    | list{ConstI32(a), ConstI32(b), RemI32Unsigned, ...tl} => go(list{ConstI32(mod(a, b)), ...tl})
    | list{ConstI32(a), ConstI32(b), ShlI32, ...tl} => go(list{ConstI32(lsl(a, b)), ...tl})
    | list{ConstI32(a), ConstI32(b), ShrI32Unsigned, ...tl} => go(list{ConstI32(lsr(a, b)), ...tl})
    | list{ConstI32(0), EqI32, ...tl} => go(list{EqzI32, ...tl})
    | list{ConstI32(a), ConstI32(b), EqI32, ...tl} => go(list{ConstI32(a == b ? 1 : 0), ...tl})
    | list{ConstI32(a), ConstI32(b), NeI32, ...tl} => go(list{ConstI32(a == b ? 0 : 1), ...tl})
    | list{ConstI32(funcIdx), CallIndirect(_, _), ...tl} => go(list{Call(funcIdx), ...tl})
    | list{ConstI32(d), DivI32Unsigned, ...tl} if Utils.Math.isPowerOf2(d) =>
      go(list{ConstI32(Float.toInt(Js.Math.log2(Int.toFloat(d)))), ShrI32Unsigned, ...tl})
    | list{ConstI32(m), MulI32, ...tl} if Utils.Math.isPowerOf2(m) =>
      go(list{ConstI32(Float.toInt(Js.Math.log2(Int.toFloat(m)))), ShlI32, ...tl})
    | list{ConstI32(0), ShrI32Unsigned, ...tl} => go(tl)
    | list{ConstI32(0), ShlI32, ...tl} => go(tl)
    | list{h, ...tl} => aux(tl, list{h, ...acc}, maxPassesLeft)
    | list{} => {
        let opt = acc->List.reverse
        let newInstsCount = opt->List.length

        if maxPassesLeft > 1 && newInstsCount < instsCount.contents {
          instsCount := newInstsCount
          aux(opt, list{}, maxPassesLeft - 1)
        } else {
          opt
        }
      }
    }
  }

  if maxPasses > 0 {
    aux(insts->List.fromArray, list{}, maxPasses)->List.toArray
  } else {
    insts
  }
}
