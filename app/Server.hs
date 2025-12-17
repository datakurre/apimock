{-# LANGUAGE OverloadedStrings #-}
module Server where

import Lib (parseOpenApiSpec, generateMockValue, generate, matchPath)
import Data.Aeson (encode)
import Data.OpenApi (OpenApi(..), PathItem(..), Operation(..), Responses(..), Response(..), MediaTypeObject(..), Referenced(Inline), Schema(..), OpenApiType(OpenApiString))
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Monoid (mempty)
import Web.Scotty (scotty, scottyApp, get, text, status, raw, param, regex, notFound, ScottyM)
import Network.HTTP.Types.Status (status200, status404)
import Control.Monad.IO.Class (liftIO) -- For liftIO
import qualified Data.Text as T

apiMockApp :: OpenApi -> ScottyM ()
apiMockApp openApi = do
  -- Default 404 for unmatched routes
  notFound $ do
    status status404
    text "Not Found: Unmatched route"

  -- Define a generic route handler for GET requests
  get (regex "(.*)") $ do
    fullPath <- param "0" -- Get the full path from the regex match
    let maybeMatch = matchPath openApi fullPath
    case maybeMatch of
      Just (matchedPathTemplate, pathParams) -> do
        let maybePathItem = InsOrdHashMap.lookup (T.unpack matchedPathTemplate) (_openApiPaths openApi)
        case maybePathItem of
          Just pathItem -> do
            case _pathItemGet pathItem of
              Just operation -> do
                -- Instead of full mock generation, just return "OK" for now
                status status200
                text "OK"
              _ -> do
                status status404
                text "Method Not Allowed"
          _ -> do
            status status404
            text "Path not found in OpenAPI spec"
      Nothing -> do
        status status404
        text "No matching OpenAPI path found"


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
  putStrLn "Starting API Mock Server on port 3000 with dummy OpenAPI spec..."
  scotty 3000 (apiMockApp dummyOpenApi)
