open Belt

let optimize = (insts: array<Wasm.Inst.t>) => {
    open Wasm.Inst

    let rec aux = (insts: list<Wasm.Inst.t>, acc: list<Wasm.Inst.t>) => {
        switch insts {
            | list{ConstI32(_), Drop, ...tl} => aux(tl, acc)
            | list{ConstI64(_), Drop, ...tl} => aux(tl, acc)
            | list{ConstF32(_), Drop, ...tl} => aux(tl, acc)
            | list{ConstF64(_), Drop, ...tl} => aux(tl, acc)
            | list{ConstI32(a), ConstI32(b), AddI32, ...tl} => aux(list{ConstI32(a + b), ...tl}, acc)
            | list{ConstI32(a), ConstI32(b), MulI32, ...tl} => aux(list{ConstI32(a * b), ...tl}, acc)
            | list{ConstI32(a), ConstI32(b), SubI32, ...tl} => aux(list{ConstI32(a - b), ...tl}, acc)
            | list{ConstI32(a), ConstI32(b), DivI32Unsigned, ...tl} => aux(list{ConstI32(a / b), ...tl}, acc)
            | list{h, ...tl} => aux(tl, list{h, ...acc})
            | list{} => acc
        }
    }

    aux(insts->List.fromArray, list{})->List.toArray->Array.reverse
}