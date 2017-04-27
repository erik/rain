all:
	elm-make src/Main.elm --output main.js

watch:
	ls src/**.elm | entr make all

