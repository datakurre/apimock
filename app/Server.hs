{-# LANGUAGE OverloadedStrings #-}
module Server where

import Lib (parseOpenApiSpec, generateMockValue, generate, matchPath)
import Data.Aeson (encode)
import Data.OpenApi (OpenApi(..), PathItem(..), Operation(..), Responses(..), Response(..), MediaTypeObject(..), Referenced(Inline))
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
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
    liftIO $ putStrLn $ "Request received for path: " ++ T.unpack fullPath
    let maybeMatch = matchPath openApi fullPath
    liftIO $ putStrLn $ "matchPath result: " ++ show maybeMatch
    case maybeMatch of
      Just (matchedPathTemplate, pathParams) -> do
        liftIO $ putStrLn $ "Matched path template: " ++ T.unpack matchedPathTemplate
        liftIO $ putStrLn $ "Path parameters: " ++ show pathParams
        -- Now find the PathItem and Operation for GET method
        let maybePathItem = InsOrdHashMap.lookup (T.unpack matchedPathTemplate) (_openApiPaths openApi)
        liftIO $ putStrLn $ "PathItem lookup result: " ++ show (fmap (\_ -> "Found PathItem") maybePathItem)
        case maybePathItem of
          Just pathItem -> do
            liftIO $ putStrLn "Found PathItem."
            case _pathItemGet pathItem of
              Just operation -> do
                liftIO $ putStrLn "Found GET operation."
                -- Instead of full mock generation, just return "OK" for now
                status status200
                text "OK"
              _ -> do
                liftIO $ putStrLn "DEBUG: No GET operation defined for this path."
                status status404
                text "Method Not Allowed"
          _ -> do
            liftIO $ putStrLn "DEBUG: Path not found in OpenAPI spec after matchPath."
            status status404
            text "Path not found in OpenAPI spec"
      Nothing -> do
        liftIO $ putStrLn "DEBUG: No matching OpenAPI path found by matchPath."
        status status404
        text "No matching OpenAPI path found"

run :: IO ()
run = do
  putStrLn "Attempting to parse openapi.json..."
  openApi <- parseOpenApiSpec "openapi.json"
  putStrLn "Successfully parsed openapi.json!"

  putStrLn "Starting API Mock Server on port 3000..."
  scotty 3000 (apiMockApp openApi)
