import React, { useState, FC, useCallback } from 'react';
import './App.css';
import { compile } from '../../src/Lib.gen';
import { Module_show } from '../../src/Compiler/Wasm.gen';
import { Split } from '@geoffcox/react-splitter';
import { Editor } from './Editor';
import { Console } from './Console';
import { Panel } from './Panel';

const SolidSplitter: FC<{}> = ({ children }) => {
  return (
    <div style={{
      background: 'silver', cursor: 'col-resize', width: '100%',
      height: '100%',
      outline: 'none',
      overflow: 'hidden'
    }}>
      {children}
    </div>
  )
};

const runModule = async (bytes: Uint8Array): Promise<any> => {
  const module = await WebAssembly.compile(bytes);
  const instance = await WebAssembly.instantiate(module);

  if ('main' in instance.exports) {
    return (instance.exports as { main: () => any }).main();
  } else {
    throw new Error('main function not found, exported functions are: ' + Object.keys(instance.exports).join(', '));
  }
};

export const isDark = (): boolean => {
  const dark = new Date().getHours() >= 20 || new Date().getHours() <= 9;
  return dark;
};

function App() {
  const [code, setCode] = useState('');
  const [output, setOutput] = useState('');
  const [isDarkMode, setDarkMode] = useState(isDark());
  const theme = isDarkMode ? 'terminal' : 'github';

  const onRun = useCallback(async () => {
    try {
      const res = compile(code);

      if (res.tag === 'Ok') {
        setOutput('running...');
        const output = await runModule(res.value[1]);
        setOutput(String(output));
      } else {
        setOutput(res.value);
      }
    } catch (error) {
      setOutput(`${error}`);
    }
  }, [code, setOutput]);

  const onDisassemble = useCallback(() => {
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
  }, [code, setOutput]);

  return (
    <div style={{ display: 'flex', flexDirection: 'column' }}>
      <Panel
        onRun={onRun}
        onDisassemble={onDisassemble}
        toggleDarkMode={() => setDarkMode(!isDarkMode)}
        isDark={isDarkMode}
        onSelectSample={code => setCode(code)}
      />
      <Split splitterSize='10px' renderSplitter={() => <SolidSplitter />}>
        <Editor theme={theme} code={code} onChange={setCode} />
        <Console theme={theme} text={output} />
      </Split>
    </div>
  );
}

export default App;