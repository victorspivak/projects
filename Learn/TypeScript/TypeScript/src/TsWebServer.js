///<reference path='node.d.ts' />
var http = require("http");

http.createServer(function (req, res) {
    res.writeHead(200, { 'Content-Type': 'text/plain' });

    //    res.end('Hello World TS modified 1\n');
    res.end(0);
}).listen(80, '127.0.0.1');
console.log('Server running at http://127.0.0.1:80/');

