import React, { FC } from 'react';
import { Button } from './Button';
import { SampleSelector } from './SampleSelector';

interface PanelProps {
  onRun: () => void,
  onDisassemble: () => void,
  isDark: boolean,
  toggleDarkMode: () => void,
  onSelectSample: (code: string) => void
}

export const Panel: FC<PanelProps> = ({ onRun, onDisassemble, isDark, toggleDarkMode, onSelectSample }) => {
  return <div
    style={{
      display: 'flex',
      flexDirection: 'row',
      backgroundColor: isDark ? 'black' : 'white',
      borderBottom: `2px solid silver`
    }}
  >
    <Button isDark={isDark} onClick={onRun}>Run</Button>
    <Button isDark={isDark} onClick={onDisassemble}>Disassemble</Button>
    <SampleSelector isDark={isDark} onSelect={onSelectSample} defaultSample='Prime sieve' />
    <Button isDark={isDark} onClick={toggleDarkMode}>{`${isDark ? 'Light' : 'Dark'} Mode`}</Button>
  </div>;
};