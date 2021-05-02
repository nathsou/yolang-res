open Belt

let writeModule = (mod, outFile) => {
  //   let instanciate: Wasm.Vec.t => Js.Promise.t<'a> = %raw(`
  //   function(bytes) {
  //     return WebAssembly.compile(Uint8Array.from(bytes).buffer)
  //       .then(module => new WebAssembly.Instance(module, {}))
  //       .then(results => results.instance);
  //   }
  // `)

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

let decompile: string => string = %raw(`
  function(path) {
    return require('child_process').execSync("wasm-decompile " + path).toString('utf-8');
  }
`)

let run = input => {
  Parser.parse(input)->Option.mapWithDefault("could not parse input", ((prog, _)) => {
    let coreProg = prog->Array.map(Core.CoreDecl.from)
    switch Inferencer.infer(coreProg) {
    | Ok((_, subst)) => {
        Js.log(
          coreProg->Array.joinWith("\n\n", d =>
            Core.CoreDecl.show(~subst=Some(subst), d)
          ) ++ "\n\n",
        )
        // Js.log(
        //   coreProg
        //   ->Array.map(Inferencer.rewriteDecl)
        //   ->Array.joinWith("\n\n", d => Core.CoreDecl.show(~subst=None, d)) ++ "\n\n",
        // )

        let mod = Compiler.compile(coreProg->Array.map(Core.CoreDecl.subst(subst)))

        let outFile = "test.wasm"
        writeModule(mod, outFile)
        decompile(outFile)
      }
    | Error(err) => `${prog->Array.joinWith("\n\n", Ast.Decl.show)}\n\n${err}`
    }
  })
}

let prog = `
  fn test1() {
    let c = 1;
    let d = 2;
    let e = 3;
    c + d * e;
  }

  fn test2() {
    let c = 1;
    let d = 2;
    let mut e = 3;
    e = d + 1;
    let mut f = e + 2 + c;
    f = 3
  }
`

Js.log(run(prog))
