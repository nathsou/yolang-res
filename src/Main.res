open Belt

let optimize: Js.Typed_array.Uint8Array.t => Js.Promise.t<Js.Typed_array.Uint8Array.t> = %raw(`
    async function(bytes) {
      const binaryen = await import('binaryen');
      const mod = binaryen.readBinary(bytes);
      binaryen.setOptimizeLevel(2);
      mod.optimize();
      return mod.emitBinary();
    }
  `)

let readModule = (path): Js.Promise.t<Js.Typed_array.Uint8Array.t> => {
  let readBytesSync: string => Js.Promise.t<Js.Typed_array.Uint8Array.t> = %raw(`
    async function(path) {
      const fs = await import('fs');
      return Uint8Array.from(fs.readFileSync(path));
    }
  `)

  readBytesSync(path)
}

let writeModule = (bytes: Js.Typed_array.Uint8Array.t, outFile): Js.Promise.t<unit> => {
  let writeBytesSync: (string, Js.Typed_array.Uint8Array.t) => Js.Promise.t<unit> = %raw(`
    async function(path, bytes) {
      const fs = await import('fs');
      fs.writeFileSync(path, bytes, 'binary');
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
        .then(module => new WebAssembly.Instance(module, {
          index: {
            log: console.log,
          }
        }))
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

let run = (input, output, opt): Js.Promise.t<unit> => {
  switch input->Lib.compile {
  | Ok((_, bytes)) => {
      let bytes = opt ? optimize(bytes) : Js.Promise.resolve(bytes)

      Js.Promise.then_(bytes => {
        switch output {
        | Some(outFile) => bytes->writeModule(outFile)
        | None => {
            bytes->runModule
            Js.Promise.resolve()
          }
        }
      }, bytes)
    }
  | Error(err) => {
      Js.Console.error(err)
      Js.Promise.resolve()
    }
  }
}

let _ = switch Node.Process.argv->Array.sliceToEnd(2) {
| ["exec", path] => Js.Promise.then_(mod => {
    runModule(mod)
    Js.Promise.resolve()
  }, readModule(path))

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

    Js.Promise.resolve()
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
| _ => {
    Js.log("Usage: yo src.yo [-O2] [--print]")
    Js.Promise.resolve()
  }
}
