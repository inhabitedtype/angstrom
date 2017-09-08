.PHONY: all build clean test

build:
	jbuilder build --dev @install

all: build

test:
	jbuilder runtest --dev

install:
	jbuilder install --dev

uninstall:
	jbuilder uninstall

clean:
	rm -rf _build *.install

