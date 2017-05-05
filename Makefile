all:
	elm-make src/Main.elm --output main.js --warn

watch:
	ls src/**.elm | entr make all

