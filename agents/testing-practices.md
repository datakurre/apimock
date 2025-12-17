# Testing Practices

## Overview
This project follows Test-Driven Development (TDD) with Behavior-Driven Development (BDD) style test descriptions for enhanced readability and clarity.

## Test-Driven Development (TDD)

### Core Principles
1. **Red-Green-Refactor Cycle**:
   - Write a failing test first (Red)
   - Write minimal code to make it pass (Green)
   - Refactor while keeping tests green (Refactor)

2. **Test First**: All new functionality must have tests written before implementation
3. **Small Steps**: Each test should verify one specific behavior
4. **Continuous Testing**: Run tests frequently during development

### TDD Workflow
```bash
# 1. Write a failing test
# 2. Run tests to see it fail
make test

# 3. Implement minimal code to pass
# 4. Run tests to see it pass
make test

# 5. Refactor if needed
# 6. Run tests to ensure nothing broke
make test

# 7. Commit
git add -A
git commit -m "Add feature X with tests"
```

## Behavior-Driven Development (BDD) Style

### When to Use BDD Style
BDD-style test descriptions are used in this project for:
- **Clarity**: Tests read like specifications
- **Communication**: Non-programmers can understand test intent
- **Documentation**: Tests serve as living documentation
- **Context**: Group related behaviors together

### BDD in Hspec

This project uses Hspec, which provides excellent BDD support through:

#### Describe Blocks
Group related functionality:
```haskell
describe "Path Matching" $ do
  -- tests for path matching functionality
```

#### Context Blocks
Set up specific scenarios:
```haskell
context "when matching static paths" $ do
  -- tests for static path scenarios

context "when matching parameterized paths" $ do
  -- tests for parameterized path scenarios
```

#### Should Statements
Clear, behavior-focused assertions:
```haskell
it "should match /products exactly" $ do
  matchPath spec "/products" `shouldBe` Just ("/products", Map.empty)

it "should extract a single path parameter" $ do
  matchPath spec "/users/123" 
    `shouldBe` Just ("/users/{userId}", Map.fromList [("userId", "123")])
```

### BDD Test Structure

Tests follow this structure:
```haskell
describe "Feature/Module Name" $ do
  context "given some initial state or scenario" $ do
    it "should exhibit expected behavior" $ do
      -- Arrange: Set up test data
      -- Act: Execute the code under test
      -- Assert: Verify the outcome
```

### Example from Project

```haskell
describe "API Mock Server" $ do
  context "GET requests" $ do
    it "should respond with 200 for /products" $ do
      Wai.get "/products" `Wai.shouldRespondWith` 200
    
    it "should return a valid JSON array for /products" $ do
      response <- Wai.get "/products"
      -- validation code
  
  context "error handling" $ do
    it "should respond with 404 for non-existent paths" $ do
      Wai.get "/nonexistent" `Wai.shouldRespondWith` 404
```

## Test Organization

### File Structure
- `test/Spec.hs` - Main test file with all test specifications
- `test/data/` - Test data files (OpenAPI specs, fixtures)

### Test Categories

1. **Unit Tests**: Test individual functions in isolation
   - Path matching logic
   - Mock data generation
   - Schema parsing

2. **Integration Tests**: Test components working together
   - Full HTTP request/response cycle
   - OpenAPI spec parsing and server setup

### Test Fixtures

Reusable test data should be:
- Defined once at the top of test files or in helper modules
- Named clearly (e.g., `dummyGetOperation`, `testOpenApiSpec`)
- Well-documented when complex

## Running Tests

### Basic Test Execution
```bash
make test                    # Run all tests
nix develop --command cabal test  # Direct cabal command
```

### With Coverage
```bash
make coverage               # Run tests with coverage report
```

### Watch Mode
```bash
make watch                  # Auto-reload on file changes
```

## Test Coverage Goals

- Aim for high coverage (>80%) of business logic
- Focus on meaningful tests over coverage percentage
- Cover edge cases and error paths
- Test public APIs thoroughly

See [test-coverage.md](./test-coverage.md) for coverage reporting details.

## Best Practices

### Do's
✅ Write tests before implementation (TDD)  
✅ Use descriptive BDD-style test names  
✅ Group related tests with `context` blocks  
✅ Keep tests focused on one behavior  
✅ Test edge cases and error conditions  
✅ Use meaningful test data  
✅ Clean up test code during refactoring  

### Don'ts
❌ Skip writing tests for "simple" code  
❌ Write tests after implementation  
❌ Use vague test descriptions like "it works"  
❌ Test implementation details instead of behavior  
❌ Create interdependent tests  
❌ Ignore failing tests  
❌ Commit code with failing tests  

## Hspec Resources

- [Hspec Documentation](https://hspec.github.io/)
- [Hspec-Wai for HTTP Testing](https://hackage.haskell.org/package/hspec-wai)
- [BDD with Hspec](https://hspec.github.io/writing-specs.html)
