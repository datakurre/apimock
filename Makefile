.PHONY: shell build watch serve test coverage

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

coverage:
	nix develop --command cabal test --enable-coverage
	@echo ""
	@echo "Coverage report generated. Finding .tix file..."
	@TIX_FILE=$$(find dist-newstyle -name "apimock-test.tix" | head -n 1); \
	if [ -n "$$TIX_FILE" ]; then \
		echo "Found: $$TIX_FILE"; \
		echo "Generating text report..."; \
		hpc report "$$TIX_FILE" --hpcdir=dist-newstyle/build/x86_64-linux/ghc-9.10.3/apimock-0.1.0.0/t/apimock-test/hpc; \
		echo ""; \
		echo "Generating HTML report in coverage_html/..."; \
		hpc markup "$$TIX_FILE" --hpcdir=dist-newstyle/build/x86_64-linux/ghc-9.10.3/apimock-0.1.0.0/t/apimock-test/hpc --destdir=coverage_html; \
		echo "Open coverage_html/hpc_index.html to view the report."; \
	else \
		echo "Error: Could not find .tix file"; \
		exit 1; \
	fi