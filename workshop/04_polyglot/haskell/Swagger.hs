{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Api
import Control.Lens
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Proxy
import Data.Swagger
import Servant.Swagger
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

instance ToParamSchema Limit where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerInteger
    & minimum_ ?~ 0
    & maximum_ ?~ 10000

swagger :: Swagger
swagger =
  toSwagger (Proxy @Api)
    & info . title .~ "Logging API"
    & info . description ?~ "This is a simple logging API."
    & info . version .~ apiVersion

parseArgs :: IO FilePath
parseArgs = do
  progName <- getProgName
  args <- getArgs
  case args of
    [dst] -> pure dst
    _ -> do
      hPutStrLn stderr $ "USAGE: " ++ progName ++ " OUTFILE"
      exitFailure

main :: IO ()
main = do
  dst <- parseArgs
  BL8.writeFile dst (encodePretty swagger)
