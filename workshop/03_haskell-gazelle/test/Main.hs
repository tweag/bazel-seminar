module Main where

import Foo
import Bar
import System.Exit

main =
  if foo == bar
  then do
    putStrLn "Test failure!"
    exitFailure
  else putStrLn "Green tests!"
