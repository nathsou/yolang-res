open Belt

let optimize: Js.Typed_array.Uint8Array.t => Js.Typed_array.Uint8Array.t = %raw(`
    function(bytes) {
      const binaryen = require('binaryen');
      const mod = binaryen.readBinary(bytes);
      binaryen.setOptimizeLevel(2);
      mod.optimize();
      return mod.emitBinary();
    }
  `)

let writeModule = (bytes: Js.Typed_array.Uint8Array.t, outFile): unit => {
  let writeBytesSync: (string, Js.Typed_array.Uint8Array.t) => unit = %raw(`
    function(path, bytes) {
      require('fs').writeFileSync(path, bytes, 'binary');
    }
  `)

  if Node.Fs.existsSync(outFile) {
    Node.Fs.unlinkSync(outFile)
  }

  writeBytesSync(outFile, bytes)
}

let runModule = (bytes: Js.Typed_array.Uint8Array.t) => {
  let instanciate: Js.Typed_array.Uint8Array.t => Js.Promise.t<'a> = %raw(`
    function(bytes) {
      return WebAssembly.compile(bytes.buffer)
        .then(module => new WebAssembly.Instance(module, {}))
        .then(instance => instance.exports.main);
    }
  `)

  let _ = instanciate(bytes)->Js.Promise.then_(mainFn => {
    let start = Js.Date.now()
    Js.log(mainFn())
    Js.log(`took ${Float.toString(Js.Date.now() -. start)} ms`)
    Js.Promise.resolve()
  }, _)
}

let run = (input, output, opt): unit => {
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

            let bytes = mod->Wasm.Module.encodeAsUint8Array
            let bytes = opt ? bytes->optimize : bytes
            switch output {
            | Some(outFile) => bytes->writeModule(outFile)
            | None => bytes->runModule
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
| [_, _, path, "-O2"] => {
    let prog = Node.Fs.readFileAsUtf8Sync(path)
    run(prog, None, true)
  }
| [_, _, path] => {
    let prog = Node.Fs.readFileAsUtf8Sync(path)
    run(prog, None, false)
  }
| [_, _, path, out, "-O2"] => {
    let prog = Node.Fs.readFileAsUtf8Sync(path)
    run(prog, Some(out), true)
  }
| [_, _, path, out] => {
    let prog = Node.Fs.readFileAsUtf8Sync(path)
    run(prog, Some(out), false)
  }
| _ => Js.log("Usage: yo src.yo")
}
