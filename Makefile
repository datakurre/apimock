.PHONY: shell build watch serve test

shell:
	nix develop
	$(eval TIX_FILE := $(shell find dist-newstyle -name "*.tix" | head -n 1))
	nix develop --command hpc report $(TIX_FILE) --hpcdir=dist-newstyle --srcdir=src --srcdir=app --srcdir=test
	nix develop --command hpc markup $(TIX_FILE) --hpcdir=dist-newstyle --srcdir=src --srcdir=app --srcdir=test --destdir=hpc_html_report

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