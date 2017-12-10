export function connectPorts(app) {
    let scrollStick = true;

    window.addEventListener('wheel', () => {
        let messageBuffer = document.getElementById('buffer-messages');

        const padding = 50;
        if (!messageBuffer || messageBuffer.scrollTop + messageBuffer.clientHeight >=
            messageBuffer.scrollHeight - padding) {
            scrollStick = true;
        } else {
            scrollStick = false;
        }
    });

    app.ports.refreshScrollPosition.subscribe(force => {
        // Bail if we're not at the bottom of the page (don't interrupt scrolling)
        if (!force && !scrollStick) {
            return;
        }

        // DOM isn't fully rendered when we're called, so wait for it.
        window.requestAnimationFrame(() => {
            let messageBuffer = document.getElementById('buffer-messages');
            if (messageBuffer) messageBuffer.scrollTop = messageBuffer.scrollHeight;

            // HACK: Also focus the input
            let inputLine = document.getElementById('input-line');
            if (inputLine) inputLine.focus();
        });
    });

    app.ports.sendNotification.subscribe(args => {
        let [title, message] = args;

        let notif = new Notification(title, {body: message});

        // TODO: Should select the tab that generated this notification
        notif.onclick = function() { window.focus(); };

        setTimeout(notif.close.bind(notif), 5000);
    });

    app.ports.modifyServerStore.subscribe(args => {
        let [metadata, action] = args;

        let servers = JSON.parse(window.localStorage.savedServers || '{}');

        if (action === 'STORE') {
            servers[metadata.name] = metadata;
        } else if (action === 'REMOVE') {
            delete servers[metadata.name];
        } else {
            console.error('bad action', action);
        }

        window.localStorage.savedServers = JSON.stringify(servers);
    });


    app.ports.requestScrollback.subscribe(serverName => {
        let item = window.localStorage.getItem(`scrollback.${serverName}`) || '{}';
        let channels = JSON.parse(item);
        let response = [];

        Object.keys(channels).forEach(channelName => {
            channels[channelName].forEach(line => {
                app.ports.receiveScrollback.send([serverName, channelName, line]);
            });
        });
    });


    app.ports.saveScrollback.subscribe(args => {
        let [serverName, channelName, line] = args;

        let item = window.localStorage.getItem(`scrollback.${serverName}`) || '{}';
        let channels = JSON.parse(item);
        let chan = channels[channelName] || [];

        chan.push(line);

        // TODO: this should probably be tunable
        if (chan.length >= 500) {
            chan = chan.slice(1);
        }

        channels[channelName] = chan;
        window.localStorage.setItem(`scrollback.${serverName}`, JSON.stringify(channels));
    });

    app.ports.clearScrollback.subscribe(args => {
        let [serverName, channelName] = args;
        let item = window.localStorage.getItem(`scrollback.${serverName}`) || '{}';
        let channels = JSON.parse(item);

        delete channels[channelName];

        window.localStorage.setItem(`scrollback.${serverName}`, JSON.stringify(channels));
    });

    return app;
}
