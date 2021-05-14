
const mode = {
  dark: {
    backgroundColor: '#1c1c1c',
    color: 'white',
    borderColor: '#3b3b3b'
  },
  light: {
    backgroundColor: 'white',
    color: 'black',
    borderColor: 'black'
  }
};

export const getTheme = (isDark: boolean) => {
  return {
    ...(isDark ? mode.dark : mode.light), borderWidth: '2px',
    borderStyle: 'solid',
    fontSize: '20px',
    margin: '4px',
    borderRadius: '4px'
  };
};