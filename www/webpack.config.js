// todo: remove jquery and webpack
const path = require('path');
module.exports = {
    entry: {
        "app":'./app.js',
        "css":'./app.css',
	   },
    output: {
	path: path.resolve(__dirname, 'static-files/editor/js/'),
	filename: '[name].js'
    },
    performance: {
	hints: process.env.NODE_ENV === 'production' ? "warning" : false
    },
    module: {
	rules: [
	    {
		test: /\.css$/,
		use: [
		    'style-loader',
		    'css-loader'
		]
	    }]}
}
