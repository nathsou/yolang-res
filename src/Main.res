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

let readModule = (path): Js.Typed_array.Uint8Array.t => {
  let readBytesSync: string => Js.Typed_array.Uint8Array.t = %raw(`
    function(path) {
      return Uint8Array.from(require('fs').readFileSync(path));
    }
  `)

  readBytesSync(path)
}

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

let time = (f: unit => 'a): ('a, float) => {
  let start = Js.Date.now()
  let res = f()
  (res, Js.Date.now() -. start)
}

let runModule = (bytes: Js.Typed_array.Uint8Array.t) => {
  let instanciate: Js.Typed_array.Uint8Array.t => Js.Promise.t<'a> = %raw(`
    function(bytes) {
      return WebAssembly.compile(bytes.buffer)
        .then(module => new WebAssembly.Instance(module, {}))
        .then(instance => instance.exports.main);
    }
  `)

  let _ = instanciate(bytes)->Js.Promise.then_(
    mainFn => {
      let (res, _took) = time(mainFn)
      Js.log(res)
      Js.Promise.resolve()
    },
    // Js.log(`took ${Float.toString(took)} ms`)

    _,
  )
}

let run = (input, output, opt): unit => {
  switch input->Lib.compile {
  | Ok((_, bytes)) => {
      let bytes = opt ? optimize(bytes) : bytes

      switch output {
      | Some(outFile) => bytes->writeModule(outFile)
      | None => bytes->runModule
      }
    }
  | Error(err) => Js.Console.error(err)
  }
}

switch Node.Process.argv->Array.sliceToEnd(2) {
| ["exec", path] => runModule(readModule(path))
| [path, "-O2"] => {
    let prog = Node.Fs.readFileAsUtf8Sync(path)
    run(prog, None, true)
  }
| [path, "--print"] => {
    let prog = Node.Fs.readFileAsUtf8Sync(path)
    switch prog->Lib.compile {
    | Ok((mod, _)) => Js.log(mod->Wasm.Module.show)
    | Error(err) => Js.Console.error(err)
    }
  }
| [path] => {
    let prog = Node.Fs.readFileAsUtf8Sync(path)
    run(prog, None, false)
  }
| [path, out, "-O2"] => {
    let prog = Node.Fs.readFileAsUtf8Sync(path)
    run(prog, Some(out), true)
  }
| [path, out] => {
    let prog = Node.Fs.readFileAsUtf8Sync(path)
    run(prog, Some(out), false)
  }
| _ => Js.log("Usage: yo src.yo [-O2] [--print]")
}
