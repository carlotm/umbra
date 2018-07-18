module.exports = {
    baseUrl: process.env.NODE_ENV === 'production'
        ? '/umbra/'
        : '/',
    productionSourceMap: false,
    configureWebpack: {
        entry: ['./src/main.js', './src/sass/main.scss'],
    }
}
