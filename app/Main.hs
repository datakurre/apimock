{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib (parseOpenApiSpec, generateMockValue, generate)
import Data.Aeson (encode)
import Data.OpenApi (OpenApi(..), PathItem(..), Operation(..), Responses(..), Response(..), MediaTypeObject(..), Referenced(Inline))
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  putStrLn "Attempting to parse openapi.json..."
  openApi <- parseOpenApiSpec "openapi.json"
  putStrLn "Successfully parsed openapi.json!"

  -- Extract the schema for the /hello endpoint's 200 response
  let helloPath = InsOrdHashMap.lookup "/hello" (_openApiPaths openApi)
  case helloPath of
    Just (PathItem { _pathItemGet = Just (Operation { _operationResponses = Responses { _responsesResponses = resps }}) }) ->
      case InsOrdHashMap.lookup 200 resps of
        Just (Inline (Response { _responseContent = content })) ->
          case InsOrdHashMap.lookup "application/json" content of
            Just (MediaTypeObject { _mediaTypeObjectSchema = Just (Inline responseSchema) }) -> do
              putStrLn "Generating mock data for /hello 200 response:"
              mockData <- generate (generateMockValue responseSchema)
              BL8.putStrLn (encode mockData)
            _ -> hPutStrLn stderr "Could not find application/json schema for /hello 200 response"
        _ -> hPutStrLn stderr "Could not find 200 response for /hello"
    _ -> hPutStrLn stderr "Could not find /hello path or GET operation"