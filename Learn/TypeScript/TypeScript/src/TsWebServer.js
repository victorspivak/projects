///<reference path='node.d.ts' />
var http = require("http");

http.createServer(function (req, res) {
    res.writeHead(200, { 'Content-Type': 'text/plain' });

    //    res.end('Hello World TS modified 1\n');
    //    res.end('Hello World TS modified 1\n');
    res.end("5");
}).listen(1338, '127.0.0.1');
console.log('Server running at http://127.0.0.1:1338/');

//# sourceMappingURL=TsWebServer.js.map
