shell:
	nix develop

build:
	nix build .#apimock

watch:
	nix develop -c ghcid -W --command="cabal repl"

serve: build
	./result/bin/apimock