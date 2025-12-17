{-# LANGUAGE OverloadedStrings #-}
-- | Core library for OpenAPI specification parsing and mock data generation.
--
-- This module provides functionality to parse OpenAPI 3.x specifications,
-- match incoming request paths to API endpoints, and generate realistic
-- mock data based on schema definitions.
module Lib
    ( parseOpenApiSpec
    , generateMockValue
    , FGen
    , generate
    , matchPath
    ) where

import Data.Aeson (Value(..), object, eitherDecodeFileStrict)
import qualified Data.Aeson.Key as Key
import Data.OpenApi (OpenApi(..), Schema(..), Referenced(Inline, Ref), OpenApiType(..), OpenApiItems(..))
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Function ((&))
import Data.Maybe (listToMaybe, mapMaybe)
import Fakedata (fakeText, fakeInt, fakeValue, FGen, generate)
import Data.Scientific (fromFloatDigits)

-- | Parse an OpenAPI specification from a JSON file.
--
-- Reads and decodes an OpenAPI 3.x specification from the given file path.
-- Exits with an error message if parsing fails.
--
-- @
-- openApi <- parseOpenApiSpec "openapi.json"
-- @
parseOpenApiSpec :: FilePath -> IO OpenApi
parseOpenApiSpec filePath = do
  eOpenApi <- eitherDecodeFileStrict filePath
  case eOpenApi of
    Left err -> do
      hPutStrLn stderr $ "Error parsing OpenAPI spec from " ++ filePath ++ ": " ++ err
      exitFailure
    Right openApi -> return openApi

-- | Generate a random JSON Value from an OpenAPI Schema.
--
-- Creates mock data that conforms to the given schema definition.
-- Supports basic types (string, integer, number, boolean), objects with properties,
-- and arrays. Schema references ($ref) are currently not resolved and will be
-- skipped with a placeholder null value.
--
-- @
-- mockData <- generate (generateMockValue schema)
-- @
generateMockValue :: Schema -> FGen Value
generateMockValue schema =
  case _schemaType schema of
    Just OpenApiString -> String <$> fakeText (1, 20)
    Just OpenApiInteger -> Number . fromIntegral <$> fakeInt (-100, 100)
    Just OpenApiNumber -> Number . fromFloatDigits <$> (fakeValue :: FGen Double)
    Just OpenApiBoolean -> Bool <$> fakeValue
    Just OpenApiObject -> do
      -- For objects, generate values for each property
      let properties = _schemaProperties schema
      -- Handle both inline schemas and references
      objPairs <- mapM (generatePropertyPair) (InsOrdHashMap.toList properties)
      return (object objPairs)
    Just OpenApiArray -> do
      -- For arrays, generate a list of items based on _schemaItems
      case _schemaItems schema of
        Just (OpenApiItemsObject (Inline itemSchema)) -> do
          numItems <- fakeInt (1, 5)
          Array . V.fromList <$> sequence (replicate numItems (generateMockValue itemSchema))
        Just (OpenApiItemsObject (Ref _)) -> 
          -- Schema references are not yet resolved
          return (Array V.empty)
        _ -> return (Array V.empty)
    _ -> return Null

-- | Generate a key-value pair for an object property.
-- Handles both inline schemas and references.
generatePropertyPair :: (Text, Referenced Schema) -> FGen (Key.Key, Value)
generatePropertyPair (name, Inline propSchema) = do
  mockVal <- generateMockValue propSchema
  return (Key.fromText name, mockVal)
generatePropertyPair (name, Ref _) = 
  -- Schema references are not yet resolved, use null as placeholder
  return (Key.fromText name, Null)

-- | Match an incoming request path to an OpenAPI path template.
--
-- Compares the request path against all paths defined in the OpenAPI spec
-- and returns the first match along with extracted path parameters.
--
-- Path parameters are denoted by curly braces in the template, e.g., @\/users\/{userId}@.
--
-- @
-- matchPath spec "\/users\/123" 
--   -- Returns: Just ("\/users\/{userId}", fromList [("userId", "123")])
-- @
matchPath :: OpenApi -> Text -> Maybe (Text, Map.Map Text Text)
matchPath spec requestPath =
  let
    requestSegments = T.splitOn "/" requestPath
    matchingPaths =
      InsOrdHashMap.toList (_openApiPaths spec)
      & mapMaybe (\(pathTemplate, _) ->
          let
            templateSegments = T.splitOn "/" (T.pack pathTemplate)
          in
            if length requestSegments == length templateSegments
            then
              let
                -- Try to match segments and extract parameters
                (isMatch, params) = foldl' (\(accMatch, accParams) (reqSeg, tempSeg) ->
                                              if T.isPrefixOf "{" tempSeg && T.isSuffixOf "}" tempSeg
                                              then -- This is a parameter segment
                                                let paramName = T.drop 1 (T.dropEnd 1 tempSeg)
                                                in (accMatch, Map.insert paramName reqSeg accParams)
                                              else -- This is a static segment
                                                (accMatch && (reqSeg == tempSeg), accParams)
                                          ) (True, Map.empty) (zip requestSegments templateSegments)
              in
                if isMatch then Just (T.pack pathTemplate, params) else Nothing
            else Nothing
        )
  in
    listToMaybe matchingPaths
