module Fakedata where

import System.Random (randomRIO, randomIO, Random)
import Data.Text (Text)
import qualified Data.Text as T

type FGen = IO

generate :: FGen a -> IO a
generate = id

fakeInt :: (Int, Int) -> FGen Int
fakeInt = randomRIO

fakeText :: (Int, Int) -> FGen Text
fakeText (minLen, maxLen) = do
    len <- randomRIO (minLen, maxLen)
    chars <- mapM (\_ -> randomRIO ('a', 'z')) [1..len]
    return $ T.pack chars

fakeValue :: Random a => FGen a
fakeValue = randomIO
