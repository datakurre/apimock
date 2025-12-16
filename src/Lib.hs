{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( parseOpenApiSpec
    , generateMockValue
    , FGen
    , generate
    , matchPath
    ) where

import Data.Aeson (Value(..), object, eitherDecodeFileStrict)
import qualified Data.Aeson.Key as Key
import Data.OpenApi (OpenApi(..), Schema(..), Referenced(Inline), OpenApiType(..), OpenApiItems(..))
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import qualified Data.Vector as V
import Data.Text (Text) -- Required for the 'Text' type
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Function ((&))
import Data.Maybe (listToMaybe, mapMaybe)
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

-- | Matches an incoming request path to an OpenAPI path template and extracts parameters.
matchPath :: OpenApi -> Text -> Maybe (Text, Map.Map Text Text)
matchPath spec requestPath =
  let
    -- Split the request path into segments
    requestSegments = T.splitOn "/" requestPath

    -- Iterate through all paths defined in the OpenAPI spec
    -- and try to find a match
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
    -- Return the first matching path and its parameters, if any
    listToMaybe matchingPaths
