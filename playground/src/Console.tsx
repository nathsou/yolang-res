import React, { FC } from 'react';
import AceEditor from "react-ace";
import "ace-builds/src-noconflict/mode-text";
import "ace-builds/src-noconflict/theme-github";
import "ace-builds/src-noconflict/theme-terminal";

interface ConsoleProps {
  theme: string,
  text: string
}

export const Console: FC<ConsoleProps> = ({ theme, text }) => {
  return (
    <AceEditor
      mode='text'
      width={'100vw'}
      height={'calc(100vh - 39px)'}
      value={text}
      theme={theme}
      enableLiveAutocompletion={true}
      onChange={() => null}
      fontSize={16}
      editorProps={{ $blockScrolling: true }}
      setOptions={{ showLineNumbers: true, tabSize: 2 }}
    />);
};