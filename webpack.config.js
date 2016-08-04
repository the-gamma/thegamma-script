var path = require("path");
var webpack = require("webpack");

var cfg = {
  devtool: "source-map",
  entry: "./out/js/main/main.js",
  output: {
    path: path.join(__dirname, "out/build"),
    filename: "bundle.js"
  },
  externals: [ { "monaco":true } ], 
  resolve: {
    root: "../out/js/"
  },
  module: {
    preLoaders: [
      {
        test: /\.js$/,
        exclude: /node_modules/,
        loader: "source-map-loader"
      }
    ]
  }
};

module.exports = cfg;