# Project Description

This project is a REST API service designed to read a given OpenAPI specification and serve mock data based on the defined endpoints and schemas. It aims to provide a lightweight and flexible solution for simulating API behavior during development and testing, without requiring a live backend.

## Key Features

-   **OpenAPI Specification Driven:** Configure the mock API directly from an OpenAPI (formerly Swagger) specification file.
-   **Mock Data Generation:** Automatically generates mock data conforming to the schemas defined in the OpenAPI spec.
-   **Endpoint Simulation:** Serves responses for all defined HTTP methods and paths (GET, POST, PUT, DELETE, PATCH).
-   **Path Parameter Matching:** Supports parameterized paths like `/users/{userId}` with proper parameter extraction.
-   **Automatic Schema-based Mock Generation:** Uses `fakedata` library to generate realistic mock data from OpenAPI schemas.
-   **Fast Development Cycle:** Enables frontend and other service development to proceed independently of backend availability.

## Current Implementation Status

The project is now functionally complete for basic mock API serving:

1. **OpenAPI Parsing:** Reads and parses OpenAPI 3.x specifications from JSON files
2. **Path Matching:** Implements intelligent path matching with support for path parameters (e.g., `/users/{userId}`)
3. **HTTP Method Support:** Handles GET, POST, PUT, DELETE, and PATCH methods
4. **Mock Data Generation:** Generates realistic mock data based on OpenAPI schemas:
   - String, Integer, Number, Boolean types
   - Objects with properties
   - Arrays of items
5. **Response Status Codes:** Returns appropriate status codes (200 for GET, 201 for POST, 204 for DELETE, etc.)
6. **Test Coverage:** Comprehensive test suite covering path matching, response generation, and HTTP method handling

## Architecture

The project is organized into several key modules:

### Core Modules

- **`Lib.hs`**: Contains core logic for OpenAPI parsing, path matching, and mock data generation
  - `parseOpenApiSpec`: Parses OpenAPI JSON files
  - `matchPath`: Matches incoming request paths to OpenAPI path templates
  - `generateMockValue`: Generates mock data from OpenAPI schemas using the `fakedata` library

- **`Fakedata.hs`**: Simple wrapper around random data generation
  - Provides `FGen` type and helper functions for generating fake data

- **`Server.hs`**: Scotty-based web server implementation
  - `apiMockApp`: Main application setup with route handlers
  - `generateMockResponse`: Helper function to generate mock responses from operations
  - `handleMethod`: Generic handler for processing HTTP methods

- **`Main.hs`**: Application entry point that starts the server

### Testing

- **`test/Spec.hs`**: Comprehensive test suite using Hspec and Hspec-Wai
  - Path matching tests
  - HTTP method tests
  - JSON response validation

### Data Flow

1. Server starts by loading `openapi.json` from disk
2. Incoming HTTP requests are matched against OpenAPI path templates
3. Path parameters are extracted (e.g., `{userId}` from `/users/123`)
4. Appropriate operation is selected based on HTTP method
5. Response schema is extracted from operation's 200/201 response
6. Mock data is generated using the schema
7. JSON response is returned with appropriate status code

## Development Environment Notes

During the initial setup, significant challenges were encountered in establishing a working Haskell development environment using both Nix flakes and Stack. Both approaches led to persistent build errors (e.g., `cabal2nix` not finding .cabal files, GHC installation failures due to `FP_PROG_LD_BUILD_ID` errors and permission denied issues). These indicate underlying environmental complexities or conflicts between Nix, Stack, and system-level dependencies. Further investigation into the host environment's Haskell toolchain setup is required for smooth development.

## Known Limitations and Future Enhancements

### Current Limitations

1. **Schema References**: The implementation currently only handles inline schemas (`Inline`). Schema references (`$ref`) are not resolved.
2. **Request Validation**: The server does not validate incoming request bodies or query parameters.
3. **Custom Examples**: OpenAPI `example` fields are not used; all data is randomly generated.
4. **Configuration**: The OpenAPI spec file path is hardcoded to `openapi.json`.
5. **Error Handling**: Limited error handling for malformed OpenAPI specs or invalid schemas.

### Potential Enhancements

1. **Schema Reference Resolution**: Implement `$ref` resolution to support component schemas
2. **Request Validation**: Add validation for request bodies, query parameters, and headers
3. **Example Support**: Use OpenAPI examples when available instead of random generation
4. **Configurable Port and File Path**: Allow command-line arguments for port and spec file
5. **Response Variants**: Support different response codes (400, 404, 500) based on configuration
6. **State Management**: Add optional state management for CRUD-like operations
7. **CORS Support**: Add CORS headers for browser-based clients
8. **OpenAPI 2.x Support**: Add backward compatibility with Swagger 2.0 specifications

## Haskell Development Best Practices with Nix

To ensure a robust and reproducible development environment for Haskell, adherence to Nix Community best practices (specifically within `nixpkgs`/NixOS) is paramount. This includes:

-   **Flake-based Development**: Utilizing `flake.nix` for defining development shells (`devShells`) and packages. This ensures consistent environments across different machines.
-   **`haskell.nix` Integration**: For complex Haskell projects, consider integrating `haskell.nix` to leverage its advanced capabilities for dependency management, multiple GHC versions, and robust builds.
-   **Pinning `nixpkgs`**: Always pin `nixpkgs` inputs to a specific commit or version to prevent unexpected breakage from `nixpkgs-unstable`.
-   **Explicit Dependencies**: Declare all Haskell dependencies explicitly in the `.cabal` file (or `package.yaml` for `hpack` projects) and ensure they are reflected in the Nix expressions.
-   **Build Reproducibility**: Prioritize `nix build` for reproducible artifacts over `cabal build` or `stack build` outside a controlled Nix environment.
-   **Debugging `nix build` failures**: When `nix build` fails, always inspect the build log (`nix log <derivation-hash>`) and trace (`--show-trace`) to understand the exact failure point. Issues often stem from incorrect `src` paths, missing `buildInputs`, or misconfigured phases.
-   **Avoid `impure` environments**: Strive to keep development and build environments as pure as possible. Use `--impure` only when strictly necessary for debugging or specific external dependencies.
