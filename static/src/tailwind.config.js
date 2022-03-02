module.exports = {
  content: ['./frontend/**/*.hs'],
  theme: {
    extend: {
      width: {
        '90' : '360px',
      },
      colors: {
        primary: {
          DEFAULT: '#00ccc0',
        },
        secondary: {
          DEFAULT: '#b8f0d5',
          'end': '#b8f0ed',
        },
        tertiary: {
          DEFAULT: '#dcf2ed',
          'end': '#d3d9ec',
        },
      },
      boxShadow: {
        'md' : '3px 3px 8px rgba(0, 0, 0, 0.08)',
      },
    },
  },
  plugins: [],
}
