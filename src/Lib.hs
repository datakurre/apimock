{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( parseOpenApiSpec
    , generateMockValue
    , FGen
    , generate
    ) where

import Data.Aeson (Value(..), object, eitherDecodeFileStrict)
import qualified Data.Aeson.Key as Key
import Data.OpenApi (OpenApi(..), Schema(..), Referenced(Inline), OpenApiType(..), OpenApiItems(..))
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import qualified Data.Vector as V
import Fakedata (fakeText, fakeInt, fakeValue, FGen, generate)
import Data.Scientific (fromFloatDigits)

-- Function to parse an OpenAPI specification from a file
parseOpenApiSpec :: FilePath -> IO OpenApi
parseOpenApiSpec filePath = do
  eOpenApi <- eitherDecodeFileStrict filePath
  case eOpenApi of
    Left err -> do
      hPutStrLn stderr $ "Error parsing OpenAPI spec from " ++ filePath ++ ": " ++ err
      exitFailure
    Right openApi -> return openApi

-- | Generate a random JSON Value from an OpenAPI Schema
generateMockValue :: Schema -> FGen Value
generateMockValue schema =
  case _schemaType schema of
    Just OpenApiString -> String <$> fakeText (1, 20) -- Generate a string
    Just OpenApiInteger -> Number . fromIntegral <$> fakeInt (-100, 100) -- Generate an integer
    Just OpenApiNumber -> Number . fromFloatDigits <$> (fakeValue :: FGen Double) -- Generate a float
    Just OpenApiBoolean -> Bool <$> fakeValue -- Generate a boolean
    Just OpenApiObject -> do
      -- For objects, generate values for each property
      let properties = _schemaProperties schema
      -- NOTE: This pattern match is non-exhaustive and only handles 'Inline' schemas.
      -- 'Ref' references are not resolved for property schemas, which might lead to runtime errors
      -- if the OpenAPI spec contains '$ref' in property definitions.
      -- For a mock server, 'Ref's would ideally be resolved to their actual schema definitions.
      objPairs <- mapM (\(name, Inline propSchema) -> do
                           mockVal <- generateMockValue propSchema
                           return (Key.fromText name, mockVal)) (InsOrdHashMap.toList properties)
      return (object objPairs)
    Just OpenApiArray -> do
      -- For arrays, generate a list of items based on _schemaItems
      case _schemaItems schema of
        Just (OpenApiItemsObject (Inline itemSchema)) -> do
          numItems <- fakeInt (1, 5) -- Generate 1 to 5 items
          Array . V.fromList <$> sequence (replicate numItems (generateMockValue itemSchema))
        _ -> return (Array V.empty) -- Default to empty array if item schema is not specified
    _ -> return Null -- Default to Null for unsupported types or Nothing
