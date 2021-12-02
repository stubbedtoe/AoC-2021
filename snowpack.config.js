// Snowpack Configuration File
// See all supported options: https://www.snowpack.dev/reference/configuration

/** @type {import("snowpack").SnowpackUserConfig } */
module.exports = {
  mount: {
    src: '/dist',
    public: '/'
  },
  plugins: [
    ["snowpack-plugin-elm", { "verbose": true, "debugger": "dev", "optimize": "build" }]
  ],
  packageOptions: {
    /* ... */
  },
  devOptions: {
    output: "stream"
  },
  buildOptions: {
    /* ... */
  },
};
