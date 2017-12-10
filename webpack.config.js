var path = require('path');

module.exports = {
    entry: './src/index.js',

    output: { path: __dirname, filename: 'bundle.js' },

    devtool: 'cheap-source-map',

    module: {
        noParse: [/\.elm$/],
        loaders: [
            {
                test: /\.js$/,
                loaders: ['babel-loader']
            },
            {
                test: /\.html$/,
                exclude: /node_modules/,
                loader: 'file-loader?name=[name].[ext]'
            },
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                loader: 'elm-webpack-loader',
                options: { warn: true }
            }
        ]
    },

    devServer: { inline: true }
};
