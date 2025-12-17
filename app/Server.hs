{-# LANGUAGE OverloadedStrings #-}
module Server where

import Lib (parseOpenApiSpec, generateMockValue, generate, matchPath)
import Data.Aeson (encode, object)
import Data.OpenApi (OpenApi(..), PathItem(..), Operation(..), Responses(..), Response(..), MediaTypeObject(..), Referenced(Inline), Schema(..), OpenApiType(OpenApiString))
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Monoid (mempty)
import Web.Scotty (scotty, scottyApp, get, post, put, delete, patch, text, status, raw, param, regex, notFound, ScottyM, ActionM)
import Network.HTTP.Types.Status (status200, status201, status404)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T

-- Helper function to generate mock response for a given operation and status code
generateMockResponse :: Operation -> Int -> ActionM ()
generateMockResponse operation statusCode = do
  let responses = _operationResponses operation
  let response = InsOrdHashMap.lookup statusCode (_responsesResponses responses)
  case response of
    Just (Inline resp) -> do
      let content = _responseContent resp
      let maybeJsonContent = InsOrdHashMap.lookup "application/json" content
      case maybeJsonContent of
        Just mediaType -> do
          case _mediaTypeObjectSchema mediaType of
            Just (Inline schema) -> do
              -- Generate mock data from schema
              mockData <- liftIO $ generate (generateMockValue schema)
              status $ toEnum statusCode
              raw $ encode mockData
            _ -> do
              -- No inline schema, return empty object
              status $ toEnum statusCode
              raw $ encode (object [])
        _ -> do
          -- No application/json content
          status $ toEnum statusCode
          text "OK"
    _ -> do
      -- No matching response defined, default to OK
      status $ toEnum statusCode
      text "OK"

-- Helper function to handle a route with a specific method
handleMethod :: OpenApi -> T.Text -> (PathItem -> Maybe Operation) -> Int -> ActionM ()
handleMethod openApi fullPath getOperation defaultStatus = do
  let maybeMatch = matchPath openApi fullPath
  case maybeMatch of
    Just (matchedPathTemplate, pathParams) -> do
      let maybePathItem = InsOrdHashMap.lookup (T.unpack matchedPathTemplate) (_openApiPaths openApi)
      case maybePathItem of
        Just pathItem -> do
          case getOperation pathItem of
            Just operation -> generateMockResponse operation defaultStatus
            _ -> do
              status status404
              text "Method Not Allowed"
        _ -> do
          status status404
          text "Path not found in OpenAPI spec"
    Nothing -> do
      status status404
      text "No matching OpenAPI path found"

apiMockApp :: OpenApi -> ScottyM ()
apiMockApp openApi = do
  -- Define route handlers for different HTTP methods
  get (regex "(.*)") $ do
    fullPath <- param "0"
    handleMethod openApi fullPath _pathItemGet 200

  post (regex "(.*)") $ do
    fullPath <- param "0"
    handleMethod openApi fullPath _pathItemPost 201

  put (regex "(.*)") $ do
    fullPath <- param "0"
    handleMethod openApi fullPath _pathItemPut 200

  delete (regex "(.*)") $ do
    fullPath <- param "0"
    handleMethod openApi fullPath _pathItemDelete 204

  patch (regex "(.*)") $ do
    fullPath <- param "0"
    handleMethod openApi fullPath _pathItemPatch 200

  -- Default 404 for unmatched routes
  notFound $ do
    status status404
    text "Not Found: Unmatched route"

-- Dummy Response for 200 status
dummy200Response :: Referenced Response
dummy200Response = Inline $ Response
  { _responseDescription = "dummy"
  , _responseContent = InsOrdHashMap.fromList
      [ ("application/json", MediaTypeObject
          { _mediaTypeObjectSchema = Just (Inline (Schema
                                                            { _schemaType = Just OpenApiString
                                                            , _schemaFormat = Nothing
                                                            , _schemaDescription = Nothing
                                                            , _schemaEnum = Nothing
                                                            , _schemaMinimum = Nothing
                                                            , _schemaMaximum = Nothing
                                                            , _schemaExclusiveMinimum = Nothing
                                                            , _schemaExclusiveMaximum = Nothing
                                                            , _schemaMaxLength = Nothing
                                                            , _schemaMinLength = Nothing
                                                            , _schemaPattern = Nothing
                                                            , _schemaMaxItems = Nothing
                                                            , _schemaMinItems = Nothing
                                                            , _schemaUniqueItems = Nothing
                                                            , _schemaMaxProperties = Nothing
                                                            , _schemaMinProperties = Nothing
                                                            , _schemaRequired = []
                                                            , _schemaProperties = InsOrdHashMap.empty
                                                            , _schemaAdditionalProperties = Nothing
                                                            , _schemaDiscriminator = Nothing
                                                            , _schemaReadOnly = Nothing
                                                            , _schemaWriteOnly = Nothing
                                                            , _schemaXml = Nothing
                                                            , _schemaExternalDocs = Nothing
                                                            , _schemaExample = Nothing
                                                            , _schemaDeprecated = Nothing
                                                            , _schemaTitle = Nothing
                                                            , _schemaDefault = Nothing
                                                            , _schemaNullable = Nothing
                                                            , _schemaAllOf = Nothing
                                                            , _schemaAnyOf = Nothing
                                                            , _schemaOneOf = Nothing
                                                            , _schemaNot = Nothing
                                                            , _schemaItems = Nothing
                                                            }))
          , _mediaTypeObjectExamples = InsOrdHashMap.empty
          }) ]
  , _responseHeaders = mempty
  , _responseLinks = mempty
  }

-- Dummy Operation with 200 response
dummyGetOperation :: Operation
dummyGetOperation = Operation
  { _operationResponses = Responses { _responsesResponses = InsOrdHashMap.fromList
      [ (200, dummy200Response) ] }
  , _operationTags = mempty
  , _operationSummary = Nothing
  , _operationDescription = Nothing
  , _operationExternalDocs = Nothing
  , _operationOperationId = Nothing
  , _operationParameters = []
  , _operationRequestBody = Nothing
  , _operationCallbacks = mempty
  , _operationDeprecated = Nothing
  , _operationSecurity = mempty
  , _operationServers = []
  }

-- Dummy OpenAPI specification for testing
dummyOpenApi :: OpenApi
dummyOpenApi = mempty
  { _openApiPaths = InsOrdHashMap.fromList
      [ ("/products", PathItem
                          { _pathItemGet = Just dummyGetOperation
                          , _pathItemPut = Nothing
                          , _pathItemPost = Nothing
                          , _pathItemDelete = Nothing
                          , _pathItemOptions = Nothing
                          , _pathItemHead = Nothing
                          , _pathItemPatch = Nothing
                          , _pathItemTrace = Nothing
                          , _pathItemServers = []
                          , _pathItemParameters = []
                          , _pathItemSummary = Nothing
                          , _pathItemDescription = Nothing
                          })
      ]
  }

run :: IO ()
run = do
  putStrLn "Loading OpenAPI specification from openapi.json..."
  openApi <- parseOpenApiSpec "openapi.json"
  putStrLn "Starting API Mock Server on port 3000..."
  scotty 3000 (apiMockApp openApi)

