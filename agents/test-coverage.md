# Test Coverage

## Overview
The project uses Haskell Program Coverage (HPC) for test coverage reporting. Coverage is integrated with Cabal's test suite and can be generated with a simple make command.

## Generating Coverage Reports

### Command Line
```bash
make coverage
```

This will:
1. Run all tests with coverage enabled
2. Generate a text coverage report to the terminal
3. Generate an HTML coverage report in `coverage_html/`
4. Open `coverage_html/hpc_index.html` to view detailed coverage by module

### What's Covered
The coverage reports show:
- **Library modules**: `Lib.hs`, `Fakedata.hs`
- **Line coverage**: Which lines of code were executed
- **Expression coverage**: Which expressions were evaluated
- **Alternative coverage**: Which pattern match alternatives were taken

### CI Integration
Coverage is automatically generated in GitHub Actions. See `.github/workflows/` for CI configuration.

## Current Coverage

As of the latest test run, the project has comprehensive test coverage including:
- Path matching logic with various scenarios
- HTTP method handling (GET, POST, PUT, DELETE, PATCH)
- Mock data generation for different OpenAPI schema types
- JSON response validation

## Known Limitations

1. **HPC Mix File Paths**: The hpc tool sometimes has difficulty finding .mix files with the default Cabal paths. The Makefile targets use specific paths to work around this.

2. **Server Module**: The Server module is rebuilt in the test suite rather than imported from the library, which affects coverage reporting slightly.

3. **Dummy Test Fixtures**: Coverage includes test-only code that exists in the Server module for testing purposes.

## Improving Coverage

To add coverage for uncovered areas:
1. Run `make coverage` to see current coverage
2. Open `coverage_html/hpc_index.html` to see detailed line-by-line coverage
3. Add tests for uncovered code paths
4. Re-run coverage to verify improvements

## Alternative: Cabal Built-in Coverage

Cabal automatically generates coverage reports in:
```
dist-newstyle/build/x86_64-linux/ghc-9.10.3/apimock-0.1.0.0/t/apimock-test/hpc/vanilla/html/hpc_index.html
```

You can view this directly after running:
```bash
nix develop --command cabal test --enable-coverage
```
