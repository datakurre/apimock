{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-} -- For json macro
module Main where

import Test.Hspec
import Test.Hspec.Wai (with, get, post, shouldRespondWith, liftIO)
import Test.Hspec.Wai as Wai -- For withWaiApp, get, post, shouldRespondWith
import Test.Hspec.Wai.JSON
import Network.Wai.Test (simpleBody)
import Data.OpenApi (OpenApi(..), PathItem(..), Operation(..), Responses(..), Referenced(Inline), MediaTypeObject(..), Schema(..))
import qualified Data.OpenApi as OpenApi -- For Response, _responseDescription, etc.
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T -- For T.pack
import Lib (matchPath, parseOpenApiSpec)
import Server (apiMockApp)
import Web.Scotty (scottyApp)
import Data.Aeson (decode, Value(..))
import Data.Maybe (isJust)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Network.Wai as Wai -- For Response type
import Data.Monoid (mempty)

-- Dummy Response for 200 status
dummy200Response :: Referenced OpenApi.Response
dummy200Response = Inline $ OpenApi.Response
  { OpenApi._responseDescription = "dummy"
  , OpenApi._responseContent = InsOrdHashMap.fromList
      [ ("application/json", MediaTypeObject
          { OpenApi._mediaTypeObjectSchema = Just (Inline (OpenApi.Schema
                                                            { OpenApi._schemaType = Just OpenApi.OpenApiString
                                                            , OpenApi._schemaFormat = Nothing
                                                            , OpenApi._schemaDescription = Nothing
                                                            , OpenApi._schemaEnum = Nothing
                                                            , OpenApi._schemaMinimum = Nothing
                                                            , OpenApi._schemaMaximum = Nothing
                                                            , OpenApi._schemaExclusiveMinimum = Nothing
                                                            , OpenApi._schemaExclusiveMaximum = Nothing
                                                            , OpenApi._schemaMaxLength = Nothing
                                                            , OpenApi._schemaMinLength = Nothing
                                                            , OpenApi._schemaPattern = Nothing
                                                            , OpenApi._schemaMaxItems = Nothing
                                                            , OpenApi._schemaMinItems = Nothing
                                                            , OpenApi._schemaUniqueItems = Nothing
                                                            , OpenApi._schemaMaxProperties = Nothing
                                                            , OpenApi._schemaMinProperties = Nothing
                                                            , OpenApi._schemaRequired = []
                                                            , OpenApi._schemaProperties = InsOrdHashMap.empty
                                                            , OpenApi._schemaAdditionalProperties = Nothing
                                                            , OpenApi._schemaDiscriminator = Nothing
                                                            , OpenApi._schemaReadOnly = Nothing
                                                            , OpenApi._schemaWriteOnly = Nothing
                                                            , OpenApi._schemaXml = Nothing
                                                            , OpenApi._schemaExternalDocs = Nothing
                                                            , OpenApi._schemaExample = Nothing
                                                            , OpenApi._schemaDeprecated = Nothing
                                                            , OpenApi._schemaTitle = Nothing
                                                            , OpenApi._schemaDefault = Nothing
                                                            , OpenApi._schemaNullable = Nothing
                                                            , OpenApi._schemaAllOf = Nothing
                                                            , OpenApi._schemaAnyOf = Nothing
                                                            , OpenApi._schemaOneOf = Nothing
                                                            , OpenApi._schemaNot = Nothing
                                                            , OpenApi._schemaItems = Nothing
                                                            }))
          , OpenApi._mediaTypeObjectExamples = InsOrdHashMap.empty
          }) ]
  , OpenApi._responseHeaders = mempty
  , OpenApi._responseLinks = mempty
  }

-- Dummy Operation with 200 response
dummyGetOperation :: Operation
dummyGetOperation = Operation
  { _operationResponses = Responses { OpenApi._responsesResponses = InsOrdHashMap.fromList
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

main :: IO ()
main = hspec $ do
  describe "Path Matching" $ do
    let openApiSpec = mempty
          { _openApiPaths = InsOrdHashMap.fromList
              [ ("/users/{userId}", PathItem
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
              , ("/products", PathItem
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
              , ("/items/{itemId}/details", PathItem
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
    
    context "when matching static paths" $ do
      it "should match /products exactly" $ do
        matchPath openApiSpec "/products" `shouldBe` Just ("/products", Map.empty)
    
    context "when matching parameterized paths" $ do
      it "should extract a single path parameter" $ do
        matchPath openApiSpec "/users/123" 
          `shouldBe` Just ("/users/{userId}", Map.fromList [("userId", "123")])
      
      it "should extract multiple path parameters" $ do
        matchPath openApiSpec "/items/abc/details" 
          `shouldBe` Just ("/items/{itemId}/details", Map.fromList [("itemId", "abc")])
    
    context "when path does not match" $ do
      it "should return Nothing for non-existent paths" $ do
        matchPath openApiSpec "/orders" `shouldBe` Nothing
      
      it "should return Nothing when path has extra segments" $ do
        matchPath openApiSpec "/users/123/extra" `shouldBe` Nothing

  -- Server tests
  openApi <- runIO $ parseOpenApiSpec "test/data/openapi.json"
  Wai.with (scottyApp $ apiMockApp openApi) $ do
    describe "API Mock Server" $ do
      
      context "GET requests" $ do
        it "should respond with 200 for /products" $ do
          Wai.get "/products" `Wai.shouldRespondWith` 200
        
        it "should return a valid JSON array for /products" $ do
          response <- Wai.get "/products"
          liftIO $ do
            let body = simpleBody response
            let maybeJson = decode body :: Maybe Value
            maybeJson `shouldSatisfy` isJust
            case maybeJson of
              Just (Array _) -> return ()
              _ -> expectationFailure "Expected JSON array"
      
      context "POST requests" $ do
        it "should respond with 201 for /products" $ do
          Wai.post "/products" "" `Wai.shouldRespondWith` 201
        
        it "should return a valid JSON object for /products" $ do
          response <- Wai.post "/products" ""
          liftIO $ do
            let body = simpleBody response
            let maybeJson = decode body :: Maybe Value
            maybeJson `shouldSatisfy` isJust
            case maybeJson of
              Just (Object _) -> return ()
              _ -> expectationFailure "Expected JSON object"
      
      context "error handling" $ do
        it "should respond with 404 for non-existent paths" $ do
          Wai.get "/nonexistent" `Wai.shouldRespondWith` 404
