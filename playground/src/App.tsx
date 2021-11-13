import { Playground, ButtonActions } from 'explang';
import React from 'react';
import { Module_show } from '../../src/Compiler/Wasm.gen';
import { compile } from '../../src/Lib.gen';
import { samples } from './samples';

const runModule = async (bytes: Uint8Array): Promise<string> => {
  const module = await WebAssembly.compile(bytes);
  const decoder = new TextDecoder();
  const instance = await WebAssembly.instantiate(module, {
    index: {
      log: console.log,
      expectEqual(a: any, b: any) {
        if (a !== b) {
          throw new Error(`[expectEqual] expected ${a}, got: ${b}`);
        }
      },
      printString(ptr: number, len: number) {
        const mem = instance.exports.memory as WebAssembly.Memory;
        const byteView = new Uint8Array(mem.buffer);
        const str = decoder.decode(byteView.subarray(ptr, ptr + len));
        console.log(str);
      },
    },
  });

  console.log(instance.exports);

  if ('main' in instance.exports) {
    console.time('run');
    const res = (instance.exports as { main: () => any }).main();
    console.timeEnd('run');
    return res;
  } else {
    return 'Error: main function not found, exported functions are: ' + Object.keys(instance.exports).join(', ');
  }
};

const actions: ButtonActions = {
  Run: async (code, setOutput) => {
    setOutput('running...');
    try {
      const res = compile(code);

      if (res.tag === 'Ok') {
        const output = await runModule(res.value[1]);
        setOutput(`${output}`);
      } else {
        setOutput(res.value);
      }
    } catch (error) {
      setOutput(`${error}`);
    }
  },
  Disassemble: (code, setOutput) => {
    try {
      const res = compile(code);

      if (res.tag === 'Ok') {
        const [module] = res.value;
        setOutput(Module_show(module));
      } else {
        setOutput(res.value);
      }
    } catch (error) {
      setOutput(`${error}`);
    }
  },
};

export const App = () => (
  <Playground
    actions={actions}
    samples={samples}
    aceMode='rust'
  />
);