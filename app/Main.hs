-- | Application entry point for the API mock server.
--
-- This module simply delegates to the Server module to start the mock API.
module Main where

import qualified Server as S

-- | Start the API mock server.
main :: IO ()
main = S.run
