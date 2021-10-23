import { Playground, ButtonActions } from 'explang';
import React from 'react';
import { Module_show } from '../../src/Compiler/Wasm.gen';
import { compile } from '../../src/Lib.gen';
import { samples } from './samples';

const runModule = async (bytes: Uint8Array): Promise<string> => {
  const module = await WebAssembly.compile(bytes);
  const instance = await WebAssembly.instantiate(module);

  if ('main' in instance.exports) {
    return (instance.exports as { main: () => any }).main();
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