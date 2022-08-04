module Test where

import System.Environment
import System.Process
import Control.Exception
import Control.Monad
import System.Exit
import Control.Concurrent

main :: IO ()
main = do
  [goBackendPath, hsClient] <- getArgs
  let
    port = 8889
    init = spawnProcess goBackendPath ["--port", show port]
    deinit = interruptProcessGroupOf
    act _ = do
      -- lol
      threadDelay $ 1 * 1000000
      readProcess hsClient ["add",  "foo"] ""
      readProcess hsClient ["add",  "bla"] ""
      res <- readProcess hsClient ["get"] ""
      let
        res' :: [String]
        res' = read res
      when (res' /= ["foo", "bla"]) exitFailure

  bracket init deinit act
