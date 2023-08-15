module Main where

import           Control.Concurrent.Async (AsyncCancelled (..))

asyncCancelled :: AsyncCancelled
asyncCancelled = AsyncCancelled

main :: IO ()
main = pure ()
