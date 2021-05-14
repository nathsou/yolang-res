import React, { FC, useEffect, useState } from 'react';
import { samples } from './samples';
import { getTheme } from './theme';

interface SampleSelectorProps {
  onSelect: (code: string) => void,
  isDark: boolean,
  defaultSample: keyof (typeof samples)
}

export const SampleSelector: FC<SampleSelectorProps> = ({ onSelect, isDark, defaultSample }) => {
  const [selectedSample, setSelectedSample] = useState(defaultSample);

  useEffect(() => {
    // send the source code for the default sample during the first render
    onSelect(samples[selectedSample]);
  }, []);

  return (
    <select
      value={selectedSample}
      style={getTheme(isDark)}
      onChange={e => {
        const sample = e.target.value as keyof (typeof samples);
        setSelectedSample(sample);
        onSelect(samples[sample]);
      }}
    >
      {Object.entries(samples).map(([name, source]) => (
        <option key={name}>{name}</option>
      ))}
    </select>
  );
};