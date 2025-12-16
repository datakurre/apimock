.PHONY: shell build watch serve test

shell:
	nix develop

build:
	nix build .#apimock

watch:
	nix develop -c ghcid -W --command="cabal repl"

serve: build
	./result/bin/apimock

test:
	nix develop --command cabal test