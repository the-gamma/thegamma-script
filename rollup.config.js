import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import terser from '@rollup/plugin-terser';

const baseConfig = {
  input: 'dist/main.js',
  external: ['monaco'],
  plugins: [
    resolve({ browser: true }),
    commonjs()
  ]
};

export default [
  {
    ...baseConfig,
    output: {
      file: 'dist/thegamma.js',
      format: 'umd',
      name: 'TheGamma',
      globals: { monaco: 'monaco' }
    }
  },
  {
    ...baseConfig,
    plugins: [...baseConfig.plugins, terser()],
    output: {
      file: 'dist/thegamma.min.js',
      format: 'umd',
      name: 'TheGamma',
      globals: { monaco: 'monaco' }
    }
  }
];
