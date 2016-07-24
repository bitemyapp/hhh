build:
	stack build

run: build
	stack exec hhh

watch:
	stack build --file-watch --fast 2>&1
