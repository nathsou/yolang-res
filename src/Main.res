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
        let mod = Compiler.compile(coreProg->Array.map(Core.CoreDecl.subst(subst)))

        let outFile = "test.wasm"
        writeModule(mod, outFile)
        decompile(outFile)
      }
    | Error(err) => `${prog->Array.joinWith("\n\n", Decl.show)}\n\n${err}`
    }
  })
}

let prog = `

  fn add1(a, b) {
    let one = 1 in
    a + b + one
  }

  fn main() {
    let n = 7 in
    let m = 1 in
    if 3 * 7 > 22 {
      n + m
    } else {
      n * m
    }
  }
`

Js.log(run(prog))
