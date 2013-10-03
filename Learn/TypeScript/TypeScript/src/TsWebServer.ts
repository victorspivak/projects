///<reference path='node.d.ts' />

import http = require("http")

http.createServer(function (req:http.ServerRequest, res:http.ServerResponse) {
    res.writeHead(200, {'Content-Type': 'text/plain'});
    res.write('Line 1\n');
    res.write('Line 2\n');
    res.end('Hello World TS modified 1\n');
    res.end(5);
}).listen(1338, '127.0.0.1');
console.log('Server running at http://127.0.0.1:1338/');

