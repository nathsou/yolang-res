open Belt

let writeModule = (mod, outFile) => {
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
    Js.Promise.resolve(())
  }, _)
}

let decompile: string => string = %raw(`
  function(path) {
    return require('child_process').execSync("wasm-decompile " + path).toString('utf-8');
  }
`)

let run = input => {
  Parser.parse(input)->Option.mapWithDefault("could not parse input", ((prog, _)) => {
    let coreProg = prog->Array.map(Core.CoreDecl.from)
        Js.log(
          coreProg
          ->Array.map(Inferencer.rewriteDecl)
          ->Array.joinWith("\n\n", d => Core.CoreDecl.show(~subst=None, d)) ++ "\n\n",
        )

    switch Inferencer.infer(coreProg) {
    | Ok((_, subst)) => {
        // Js.log(
        //   coreProg->Array.joinWith("\n\n", d =>
        //     Core.CoreDecl.show(~subst=Some(subst), d)
        //   ) ++ "\n\n",
        // )

        let mod = Compiler.compile(coreProg->Array.map(Core.CoreDecl.subst(subst)))

        let outFile = "test.wasm"
        writeModule(mod, outFile)
        let _ = runModule(mod)
        decompile(outFile)
      }
    | Error(err) => `${prog->Array.joinWith("\n\n", Ast.Decl.show)}\n\n${err}`
    }
  })
}

let prog = `
  fn main() {
    let mut i = 0;
    let mut sum = 0;
    let test = (3 * 7 + 1) / 2;

    while i <= 1000 {
      sum = sum + i * i;
      i = i + 1;
    };

    sum
  }
`

Js.log(run(prog))
