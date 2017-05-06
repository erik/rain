/*jshint esversion: 6 */

const tls = require('tls');
const url = require('url');

const WebSocket = require('ws');


const SUPPORTED_CAPS = [
    'znc.in/server-time-iso',
    'server-time'
];

const wss = new WebSocket.Server({ port: 6676 });


wss.on('connection', function connection(ws) {
    console.log('connection', ws);

    let query = url.parse(ws.upgradeReq.url, true).query;
    const required = ['host', 'port', 'nick'];

    for (let i in required) {
        if (!query[required[i]])
            return ws.send(`missing required param ${required[i]}`);
    }

    const socket = tls.connect({
        host: query.host,
        port: +query.port,
        rejectUnauthorized: false
    }, function () {
        console.log('connected');

        SUPPORTED_CAPS.forEach(c => {
            this.write(`CAP REQ ${c}\n`);
        });

        this.write('CAP END\n');

        if (query.pass)
            this.write(`PASS ${query.pass}\n`);

        this.write(`NICK ${query.nick}\n`);
        this.write(`USER ${query.nick} * * :${query.nick}\n`);
    })
              .setEncoding('utf8')
              .on('data', (data) => {
                  console.log(data.replace(/[\r\n]+/, ''));
                  ws.send(data);
              })
              .on('end', () => { ws.close(); });

    ws.on('message', function incoming(message) {
        console.log('received: >%s<', message);
        socket.write(message + '\n');
    }).on('close', () => socket.destroy());
});
