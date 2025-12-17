-- | Simple random data generation for mock APIs.
--
-- This module provides basic utilities for generating random test data
-- using Haskell's System.Random. The FGen type is a simple alias to IO,
-- making it easy to compose random data generators.
module Fakedata 
    ( FGen
    , generate
    , fakeInt
    , fakeText
    , fakeValue
    ) where

import System.Random (randomRIO, randomIO, Random)
import Data.Text (Text)
import qualified Data.Text as T

-- | Type alias for fake data generators.
-- Currently just an alias to IO for simplicity.
type FGen = IO

-- | Execute a fake data generator.
-- This is currently just the identity function since FGen = IO.
generate :: FGen a -> IO a
generate = id

-- | Generate a random integer within the given range (inclusive).
fakeInt :: (Int, Int) -> FGen Int
fakeInt = randomRIO

-- | Generate random lowercase text of length between minLen and maxLen.
fakeText :: (Int, Int) -> FGen Text
fakeText (minLen, maxLen) = do
    len <- randomRIO (minLen, maxLen)
    chars <- mapM (\_ -> randomRIO ('a', 'z')) [1..len]
    return $ T.pack chars

-- | Generate a random value of any type that implements Random.
fakeValue :: Random a => FGen a
fakeValue = randomIO
