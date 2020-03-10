.PHONY: all build clean test install uninstall doc examples

build:
	dune build

all: build

test:
	dune runtest

examples:
	dune build @examples

install:
	dune install

uninstall:
	dune uninstall

doc:
	dune build @doc

clean:
	rm -rf _build *.install

fmt:
	dune build @fmt --auto-promote 2> /dev/null || true
	git diff --exit-code
