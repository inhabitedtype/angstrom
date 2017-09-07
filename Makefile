.PHONY: all build clean test

build:
	jbuilder build --dev @install

all: build

test:
	jbuilder --dev runtest

install:
	jbuilder --dev install

uninstall:
	jbuilder uninstall

clean:
	rm -rf _build *.install

