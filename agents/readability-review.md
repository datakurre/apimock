# Code Readability Review

## Overview
This document outlines readability and understandability improvements for the apimock codebase based on Haskell best practices.

## Issues Identified

### 1. Missing Module Documentation (All Modules)
**Issue**: None of the modules have module-level Haddock documentation explaining their purpose.

**Impact**: New developers or AI agents need to infer module purposes from code.

**Recommendation**: Add module-level documentation with examples.

### 2. Incomplete Pattern Matching in `Lib.hs`
**Location**: `Lib.hs` - `generateMockValue` function

**Issue**: Pattern match on `Referenced` type only handles `Inline` case but not `Ref` case. Comment acknowledges this but the pattern is still non-exhaustive.

**Current Code**:
```haskell
objPairs <- mapM (\(name, Inline propSchema) -> do
```

**Impact**: Runtime crashes if OpenAPI spec contains `$ref` in property definitions.

**Recommendation**: Handle `Ref` case explicitly, even if just to skip it or log a warning.

### 3. Missing Import Documentation in `Lib.hs`
**Location**: `Lib.hs` - Line 17

**Issue**: Comment `-- Required for the 'Text' type` is redundant since it's obvious from the import.

**Recommendation**: Remove redundant comments or expand to explain why this import is structured this way.

### 4. foldl' Import Missing in `Lib.hs`
**Location**: `Lib.hs` - Line 88

**Issue**: `foldl'` is used but not imported. This suggests it's being imported implicitly or there's a missing import.

**Recommendation**: Explicitly import `foldl'` from `Data.List` or `Data.Foldable`.

### 5. Complex Nested Case Statements in `Server.hs`
**Location**: `Server.hs` - `generateMockResponse` function

**Issue**: Multiple nested case statements make the logic hard to follow.

**Recommendation**: Extract helper functions or use pattern guards/LambdaCase for cleaner flow.

### 6. Dummy Code in Production Modules
**Location**: `Server.hs` - Lines 95-159

**Issue**: `dummy200Response` and `dummyGetOperation` are defined in the production module but only used for testing.

**Recommendation**: Move these to a test utility module.

### 7. Inconsistent Error Messages
**Location**: `Server.hs` - `handleMethod` function

**Issue**: Different error messages for similar 404 scenarios:
- "Method Not Allowed" (should be 405)
- "Path not found in OpenAPI spec"
- "No matching OpenAPI path found"

**Recommendation**: Standardize error responses and use correct HTTP status codes.

### 8. Magic Numbers
**Location**: Multiple locations

**Issue**: 
- Status codes (200, 201, 204, 404) hardcoded throughout
- String length bounds (1, 20), (1, 5), (-100, 100) in `Lib.hs`

**Recommendation**: Define named constants for better maintainability.

### 9. Test Code Duplication
**Location**: `test/Spec.hs`

**Issue**: `dummy200Response` and `dummyGetOperation` are duplicated from `Server.hs`.

**Recommendation**: Create shared test fixtures module.

### 10. Minimal Function Documentation
**Location**: All modules

**Issue**: Most functions lack Haddock documentation explaining parameters, return values, and behavior.

**Recommendation**: Add comprehensive Haddock comments, especially for exported functions.

## Priority Improvements

### High Priority
1. Fix incomplete pattern matching in `generateMockValue` to handle `Ref` case
2. Add explicit import for `foldl'`
3. Fix incorrect HTTP status codes (405 vs 404)
4. Move dummy/test fixtures out of production code

### Medium Priority
5. Add module-level Haddock documentation
6. Add function-level Haddock documentation for all exported functions
7. Extract named constants for magic numbers
8. Simplify nested case statements in `generateMockResponse`

### Low Priority
9. Create shared test fixtures module
10. Remove redundant comments
