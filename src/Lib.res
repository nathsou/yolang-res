open Belt

@genType
let compile = (input: string): result<(Wasm.Module.t, Js.Typed_array.Uint8Array.t), string> => {
  try {
    Parser.parse(input)->Result.flatMap(prog => {
      let coreProg = prog->Array.map(Core.CoreDecl.from)

      Inferencer.infer(coreProg)->Result.flatMap(((_, subst)) => {
        Context.substIdentifiers(subst)
        let core = coreProg->Array.map(Core.CoreDecl.subst(subst))

        Compiler.compile(core)->Result.map(mod => {
          let bytes = mod->Wasm.Module.encodeAsUint8Array

          (mod, bytes)
        })
      })
    })
  } catch {
  | exn => Error(CompilerExn.show(exn))
  }
}
