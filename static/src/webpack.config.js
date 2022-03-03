module.exports = {
    entry: [
        './src/main.js'
    ],
    output: {
        path: __dirname + '/dist',
        filename: "app.bundle.js"
    },
    optimization: {
        minimize: false
    },
    experiments: {
        asyncWebAssembly: true
    },
    module: {
        rules: [
            {
                test: /\.js$/,
                exclude: /node_modules/,
                use: {
                    loader: "babel-loader"
                }
            },
            {
                test: /\.wasm$/,
                exclude: /node_modules/,
                use: {
                    loader: "wasm-loader"
                }
            }
        ]
    },
    // plugins: []
};
