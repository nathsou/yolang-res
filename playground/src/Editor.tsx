import React, { FC } from 'react';
import AceEditor from "react-ace";
import "ace-builds/src-noconflict/mode-rust";
import "ace-builds/src-noconflict/theme-github";
import "ace-builds/src-noconflict/theme-terminal";

interface EditorProps {
  theme: string,
  code: string,
  onChange: (newCode: string) => void
}

export const Editor: FC<EditorProps> = ({ theme, code, onChange }) => {

  return (
    <AceEditor
      mode='rust'
      width={'100vw'}
      height={'calc(100vh - 39px)'}
      value={code}
      theme={theme}
      enableLiveAutocompletion={true}
      onChange={onChange}
      fontSize={16}
      editorProps={{ $blockScrolling: true }}
    />);
};