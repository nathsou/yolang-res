open Belt

let writeModule = (mod, outFile): unit => {
  let writeBytesSync: (string, Wasm.Vec.t) => unit = %raw(`
    function(path, bytes) {
      require('fs').writeFileSync(path, Uint8Array.from(bytes), 'binary');
    }
  `)

  if Node.Fs.existsSync(outFile) {
    Node.Fs.unlinkSync(outFile)
  }

  let bytes = mod->Wasm.Module.encode
  writeBytesSync(outFile, bytes)
}

let runModule = mod => {
  let instanciate: Wasm.Vec.t => Js.Promise.t<'a> = %raw(`
    function(bytes) {
      return WebAssembly.compile(Uint8Array.from(bytes).buffer)
        .then(module => new WebAssembly.Instance(module, {}))
        .then(instance => instance.exports.main);
    }
  `)

  let _ = instanciate(mod->Wasm.Module.encode)->Js.Promise.then_(mainFn => {
    Js.log(mainFn())
    Js.Promise.resolve()
  }, _)
}

let run = (input, output): unit => {
  switch Parser.parse(input) {
  | Ok(prog) => {
      let coreProg = prog->Array.map(Core.CoreDecl.from)

      switch Inferencer.infer(coreProg) {
      | Ok((_, subst)) => {
          Context.substIdentifiers(subst)
          let core = coreProg->Array.map(Core.CoreDecl.subst(subst))

          switch Compiler.compile(core) {
          | Ok(mod) =>
            // Js.log(mod->Wasm.Module.show ++ "\n\n")
            switch output {
            | Some(outFile) => mod->writeModule(outFile)
            | None => mod->runModule
            }
          | Error(err) => Js.Console.error(err)
          }
        }
      | Error(err) =>
        Js.Console.error(
          `${prog
            ->Array.map(Core.CoreDecl.from)
            ->Array.map(Inferencer.rewriteDecl)
            ->Array.joinWith("\n\n", Core.CoreDecl.show(~subst=Some(Subst.empty)))}\n\n${err}`,
        )
      }
    }
  | Error(err) => Js.Console.error(err)
  }
}

switch Node.Process.argv {
| [_, _, path] => {
    let prog = Node.Fs.readFileAsUtf8Sync(path)
    run(prog, None)
  }
| [_, _, path, out] => {
    let prog = Node.Fs.readFileAsUtf8Sync(path)
    run(prog, Some(out))
  }
| _ => Js.log("Usage: yo src.yo")
}
