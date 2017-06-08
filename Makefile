ELM_FLAGS ?= --warn

all: build

debug: ELM_FLAGS += --debug
debug: clean build

build:
	elm-make src/Main.elm --output main.js $(ELM_FLAGS)

clean:
	rm -r elm-stuff/build-artifacts/*/user/

watch:
	ls src/**.elm | entr make all

server:
	python3 -m http.server

proxy:
	cd wsproxy; npm install; node index.js

# Spin up everything
dev:
	make -j watch server proxy
