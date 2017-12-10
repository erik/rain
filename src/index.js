/* jshint esversion: 6 */

require('./index.html');

import Elm from './Main.elm';

import { connectPorts } from './ports.js';

let node = document.getElementById('main');
let app = connectPorts(Elm.Main.embed(node));


// Ask for permission to show notifications
if (Notification.permission === 'default') {
    Notification.requestPermission();
}


// push saved server configuration
if (window.localStorage.savedServers) {
    let saved = JSON.parse(window.localStorage.savedServers);
    Object.keys(saved).forEach(k => {
        app.ports.addSavedServer.send(saved[k]);
    });
}
