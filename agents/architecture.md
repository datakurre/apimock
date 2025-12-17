# Architecture

The project is organized into several key modules:

## Core Modules

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

## Testing

- **`test/Spec.hs`**: Comprehensive test suite using Hspec and Hspec-Wai
  - Path matching tests
  - HTTP method tests
  - JSON response validation

## Data Flow

1. Server starts by loading `openapi.json` from disk
2. Incoming HTTP requests are matched against OpenAPI path templates
3. Path parameters are extracted (e.g., `{userId}` from `/users/123`)
4. Appropriate operation is selected based on HTTP method
5. Response schema is extracted from operation's 200/201 response
6. Mock data is generated using the schema
7. JSON response is returned with appropriate status code
