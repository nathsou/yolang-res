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
            | list{ConstI32(a), ConstI32(b), AddI32, ...tl} => aux(list{ConstI32(a + b), ...tl}, acc)
            | list{ConstI32(a), ConstI32(b), MulI32, ...tl} => aux(list{ConstI32(a * b), ...tl}, acc)
            | list{ConstI32(a), ConstI32(b), SubI32, ...tl} => aux(list{ConstI32(a - b), ...tl}, acc)
            | list{ConstI32(a), ConstI32(b), DivI32Unsigned, ...tl} => aux(list{ConstI32(a / b), ...tl}, acc)
            | list{ConstI32(n), ConstI32(0), EqI32, ...tl} => aux(list{ConstI32(n), EqzI32, ...tl}, acc)
            | list{ConstI32(0), ConstI32(n), EqI32, ...tl} => aux(list{ConstI32(n), EqzI32, ...tl}, acc)
            | list{ConstI32(a), ConstI32(b), EqI32, ...tl} => aux(list{ConstI32(a == b ? 1 : 0), ...tl}, acc)
            | list{ConstI32(a), ConstI32(b), NeI32, ...tl} => aux(list{ConstI32(a == b ? 0 : 1), ...tl}, acc)
            | list{h, ...tl} => aux(tl, list{h, ...acc})
            | list{} => acc->List.toArray->Array.reverse
        }
    }

    aux(insts->List.fromArray, list{})
}

let optimize = peephole