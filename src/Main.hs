module Main where

import Data.Aeson (Value(..), object, (.=), eitherDecodeFileStrict, encode)
import Data.OpenApi (OpenApi(..), Schema(..), Referenced(Inline), PathItem(..), Operation(..), Responses(..), Response(..), MediaTypeObject(..))
import qualified Data.ByteString.Lazy as BL
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import qualified Data.Map as Map
import Data.Text (Text)
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
    Just OpenApiNumber -> Number . fromFloatDigits <$> fakeValue -- Generate a float
    Just OpenApiBoolean -> Bool <$> fakeValue -- Generate a boolean
    Just OpenApiObject -> do
      -- For objects, generate values for each property
      let properties = _schemaProperties schema
      objPairs <- mapM (\(name, Referenced (Inline propSchema)) -> do
                           mockVal <- generateMockValue propSchema
                           return (name, mockVal)) (Map.toList properties)
      return (object objPairs)
    Just OpenApiArray -> do
      -- For arrays, generate a list of items based on _schemaItems
      case _schemaItems schema of
        Just (Inline itemSchema) -> do
          numItems <- fakeInt (1, 5) -- Generate 1 to 5 items
          Array <$> sequence (replicate numItems (generateMockValue itemSchema))
        _ -> return (Array []) -- Default to empty array if item schema is not specified
    _ -> return Null -- Default to Null for unsupported types or Nothing

main :: IO ()
main = do
  putStrLn "Attempting to parse openapi.json..."
  openApi <- parseOpenApiSpec "openapi.json"
  putStrLn "Successfully parsed openapi.json!"

  -- Extract the schema for the /hello endpoint's 200 response
  let helloPath = Map.lookup "/hello" (_openApiPaths openApi)
  case helloPath of
    Just (PathItem { _pathItemGet = Just (Operation { _operationResponses = Responses { _responsesResponses = resps }}) }) ->
      case Map.lookup 200 resps of
        Just (Inline (Response { _responseContent = content })) ->
          case Map.lookup "application/json" content of
            Just (MediaTypeObject { _mediaTypeObjectSchema = Just (Inline responseSchema) }) -> do
              putStrLn "Generating mock data for /hello 200 response:"
              mockData <- generate (generateMockValue responseSchema)
              BL.putStrLn (encode mockData)
            _ -> hPutStrLn stderr "Could not find application/json schema for /hello 200 response"
        _ -> hPutStrLn stderr "Could not find 200 response for /hello"
    _ -> hPutStrLn stderr "Could not find /hello path or GET operation"


