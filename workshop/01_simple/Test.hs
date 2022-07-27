module Main where

import Control.Monad (unless)
import System.Exit (exitFailure)

main :: IO ()
main =
  unless (1 + 1 == 2)
    exitFailure
