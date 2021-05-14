import React, { FC } from 'react';
import { getTheme } from './theme';

interface ButtonProps {
  onClick: () => void,
  isDark: boolean
}

export const Button: FC<ButtonProps> = ({ children, onClick, isDark }) => {
  return (
    <button
      onClick={onClick}
      style={getTheme(isDark)}
    >
      { children}
    </button >
  );
};