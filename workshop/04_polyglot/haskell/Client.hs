{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

import Api
import Data.Aeson
import Data.Proxy
import Data.Text (Text, pack, unpack)
import GHC.Generics
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Client.Internal (readPositiveInt)
import Servant.API
import Servant.Client
import System.Environment (getArgs, getProgName)

getLogs :: Maybe Limit -> ClientM [Text]
addLog :: Text -> ClientM Int
getLogs :<|> addLog = client api

allLogs = getLogs Nothing

serverUrl = BaseUrl Http "localhost" 8889 ""

run :: ClientM a -> (a -> String) -> IO ()
run req render = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM req (mkClientEnv manager' serverUrl)
  putStrLn $ case res of
    Left err -> "Error: " ++ show err
    Right res -> render res

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("get" : []) ->
      run allLogs (\a -> show $ Prelude.map unpack a)
    ("get" : limit : []) ->
      run (getLogs $ Limit <$> readPositiveInt limit) (\a -> show $ Prelude.map unpack a)
    ("add" : message : []) ->
      run (addLog $ pack message) show
    _ -> do
      prog <- getProgName
      putStrLn $ "usage: " ++ prog ++ " COMMAND\n\nCommands:\n\n  * get [LIMIT]\n\n  * add MESSAGE\n"
