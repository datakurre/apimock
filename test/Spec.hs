{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Data.OpenApi (OpenApi(..), PathItem(..))
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import qualified Data.Map as Map
import Lib (matchPath) -- Assuming matchPath is in Lib.hs

main :: IO ()
main = hspec $ do
  describe "Environment" $ do
    it "can run tests" $ do
      True `shouldBe` True

  describe "Path Matching" $ do
    let openApiSpec = mempty
          { _openApiPaths = InsOrdHashMap.fromList
              [ ("/users/{userId}", PathItem
                                        { _pathItemGet = Nothing
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
                                { _pathItemGet = Nothing
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
                                            { _pathItemGet = Nothing
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
    it "matches a static path" $ do
      matchPath openApiSpec "/products" `shouldBe` Just ("/products", Map.empty)
    it "matches a path with a single parameter and extracts it" $ do
      matchPath openApiSpec "/users/123" `shouldBe` Just ("/users/{userId}", Map.fromList [("userId", "123")])
    it "matches a path with multiple parameters and extracts them" $ do
      matchPath openApiSpec "/items/abc/details" `shouldBe` Just ("/items/{itemId}/details", Map.fromList [("itemId", "abc")])
    it "does not match a non-existent path" $ do
      matchPath openApiSpec "/orders" `shouldBe` Nothing
    it "does not match a partially matched path" $ do
      matchPath openApiSpec "/users/123/extra" `shouldBe` Nothing
