# apimock

A lightweight REST API mock server driven by OpenAPI 3.x specifications.

## Overview

`apimock` reads an OpenAPI specification file and serves mock HTTP responses based on the defined endpoints and schemas. It enables rapid development and testing by simulating API behavior without requiring a live backend.

## Features

- **OpenAPI 3.x Support**: Parse and serve endpoints from OpenAPI specifications
- **Automatic Mock Data**: Generate realistic mock data based on schema definitions
- **Path Parameters**: Full support for parameterized paths like `/users/{userId}`
- **Multiple HTTP Methods**: Handle GET, POST, PUT, DELETE, and PATCH requests
- **Schema-Based Generation**: Mock data conforms to OpenAPI schema types (string, integer, number, boolean, object, array)

## Quick Start

### Prerequisites

- [Nix](https://nixos.org/download.html) with flakes enabled

### Build and Run

```bash
# Build the project
nix build

# Run with your OpenAPI specification
./result/bin/apimock
# Expects openapi.json in current directory
```

The server starts on `http://localhost:3000` and serves mock responses for all paths defined in the OpenAPI spec.

### Example

Given an `openapi.json` with:
```json
{
  "openapi": "3.0.0",
  "paths": {
    "/products": {
      "get": {
        "responses": {
          "200": {
            "content": {
              "application/json": {
                "schema": {
                  "type": "array",
                  "items": {
                    "type": "object",
                    "properties": {
                      "id": { "type": "integer" },
                      "name": { "type": "string" }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
```

Making a request:
```bash
curl http://localhost:3000/products
```

Returns a mock JSON array with generated product data.

## Development

### Setup

```bash
# Enter development shell
nix develop

# Run tests
make test

# Run tests with coverage
make coverage

# Watch mode (auto-reload)
make watch
```

### Running Tests

Tests use Hspec with BDD-style specifications:

```bash
cabal test
```

### Project Structure

```
apimock/
├── src/              # Library source code
│   ├── Lib.hs       # OpenAPI parsing and path matching
│   └── Fakedata.hs  # Mock data generation
├── app/             # Application entry point
│   ├── Main.hs      # Main executable
│   └── Server.hs    # Scotty web server
├── test/            # Test suite
│   ├── Spec.hs      # Test specifications
│   └── data/        # Test fixtures
└── agents/          # Documentation for AI agents
```

## Limitations

- Schema `$ref` references are not currently resolved
- Request validation is not performed
- OpenAPI `example` fields are not used
- Spec file path is hardcoded to `openapi.json`

## License

Apache-2.0

## Contributing

This project follows Test-Driven Development (TDD) practices. All contributions should include tests.
