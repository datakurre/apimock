# Project Description

This project is a REST API service designed to read a given OpenAPI specification and serve mock data based on the defined endpoints and schemas. It aims to provide a lightweight and flexible solution for simulating API behavior during development and testing, without requiring a live backend.

## Key Features

-   **OpenAPI Specification Driven:** Configure the mock API directly from an OpenAPI (formerly Swagger) specification file.
-   **Mock Data Generation:** Automatically generates mock data conforming to the schemas defined in the OpenAPI spec.
-   **Endpoint Simulation:** Serves responses for all defined HTTP methods and paths (GET, POST, PUT, DELETE, PATCH).
-   **Path Parameter Matching:** Supports parameterized paths like `/users/{userId}` with proper parameter extraction.
-   **Automatic Schema-based Mock Generation:** Uses `fakedata` library to generate realistic mock data from OpenAPI schemas.
-   **Fast Development Cycle:** Enables frontend and other service development to proceed independently of backend availability.

## Documentation for Agents

Detailed documentation is organized in the `./agents/` directory:

- **[Architecture](./agents/architecture.md)**: System architecture, module organization, and data flow
- **[Implementation Status](./agents/implementation-status.md)**: Current features, limitations, and future enhancements
- **[Nix Development](./agents/nix-development.md)**: Nix setup, best practices, and environment notes
