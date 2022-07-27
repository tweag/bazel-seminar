module Main where

import Foo
import Bar
import Formatting.Examples
import qualified Data.Text.Lazy.IO

main :: IO ()
main = do
  putStrLn foo
  putStrLn bar
  Data.Text.Lazy.IO.putStrLn hello
