.PHONY: all build clean configure format haddock install lint repl test

all: install configure build test haddock

build:
	cabal build

clean:
	cabal clean
	cabal sandbox delete

configure:
	cabal configure --enable-tests

format:
	cabal format
	git ls-files '*.hs' | xargs -n 1 scan --inplace-modify
	git ls-files '*.hs' | xargs stylish-haskell --inplace

haddock:
	cabal haddock

install:
	cabal sandbox init
	cabal install --enable-tests --only-dependencies

lint:
	git ls-files '*.hs' | xargs hlint

repl:
	cabal repl lib:strive

test:
	cabal test
