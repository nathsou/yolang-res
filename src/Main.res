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
  Parser.parse(input)->Option.mapWithDefault("could not parse input", ((expr, _)) => {
    let coreExpr = Core.fromExpr(expr)
    switch Inferencer.inferCoreExprType(coreExpr) {
    | Ok((_, _)) => {
        let mod = Compiler.compile(coreExpr)

        let outFile = "test.wasm"
        writeModule(mod, outFile)
        decompile(outFile)
      }
    | Error(err) => `${coreExpr->Core.show}\n\n${err}`
    }
  })
}

Js.log(run("(if (3 * (7 + 1) >= 22) { 1 } else { 2 }) + 4"))
