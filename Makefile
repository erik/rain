all: build

build:
	elm-make src/Main.elm --output main.js --warn

clean:
	rm -r elm-stuff/build-artifacts/*/user/

watch:
	ls src/**.elm | entr make all

server:
	python3 -m http.server

proxy:
	cd wsproxy; node index.js

# Spin up everything
dev:
	make -j watch server proxy
