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

@genType
module Debug = {
  @genType
  let debugAst = (input: string): result<string, string> => {
    try {
      switch Parser.parse(input) {
      | Ok(prog) => Ok(prog->Array.joinWith("\n\n", decl => decl->Ast.Decl.show))
      | Error(err) => Error(err)
      }
    } catch {
    | exn => Error(CompilerExn.show(exn))
    }
  }

  @genType
  let debugCoreAst = (input: string): result<string, string> => {
    try {
      switch Parser.parse(input) {
      | Ok(prog) => {
          let coreProg = prog->Array.map(Core.CoreDecl.from)
          Ok(coreProg->Array.joinWith("\n\n", core => core->Core.CoreDecl.show))
        }
      | Error(err) => Error(err)
      }
    } catch {
    | exn => Error(CompilerExn.show(exn))
    }
  }

  @genType
  let debugTypes = (input: string): result<string, string> => {
    try {
      switch Parser.parse(input) {
      | Ok(prog) => {
          let coreProg = prog->Array.map(Core.CoreDecl.from)
          switch Inferencer.infer(coreProg) {
          | Ok((_, subst)) => {
              Context.substIdentifiers(subst)
              let core = coreProg->Array.map(Core.CoreDecl.subst(subst))
              Ok(core->Array.joinWith("\n\n", core => core->Core.CoreDecl.show))
            }
          | Error(err) => Error(err)
          }
        }
      | Error(err) => Error(err)
      }
    } catch {
    | exn => Error(CompilerExn.show(exn))
    }
  }
}
