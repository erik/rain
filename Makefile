all: build

debug: clean
	webpack

build:
	webpack -p --progress

clean:
	rm -r elm-stuff/build-artifacts/*/user/

watch:
	webpack --watch

server:
	webpack-dev-server

proxy:
	node websocket_proxy_server.js

# Spin up everything
dev:
	make -j server proxy
